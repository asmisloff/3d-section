using System;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.Runtime;

namespace SBTools
{
	public class MyClass
	{
		[LispFunction("sbt:get-tmatrix")]
		public static ResultBuffer GetTMatrix(ResultBuffer rbArgs)
		{
			ResultBuffer result = new ResultBuffer();
			if (rbArgs != null) {
				foreach (TypedValue tv in rbArgs) {
					if (tv.TypeCode == (int)LispDataType.ObjectId) {
						Document doc = Application.DocumentManager.MdiActiveDocument;
						Database db = doc.Database;
						ObjectId _id = (ObjectId)tv.Value;
						Transaction tr = db.TransactionManager.StartTransaction();
						using (tr) {
							DBObject o = tr.GetObject(_id, OpenMode.ForRead);
							Entity ent = o as Entity;
							if (ent == null) {
								result.Add(new TypedValue((int)TypeCode.Empty));
							}
							else {
								Matrix3d M = (ent as BlockReference).BlockTransform;
								double[] arr = M.ToArray();
								for (int i = 0, j = 3; i < arr.Length; ++i, --j) {
									if (j == 3)
										result.Add(new TypedValue((int)LispDataType.ListBegin));
									result.Add(new TypedValue((int)LispDataType.Double, arr[i]));
									if (j == 0) {
										result.Add(new TypedValue((int)LispDataType.ListEnd));
										j = 4;
									}
								}
							}
							tr.Abort();
						}
					}
				}
			}
			return result;
		}
	}
}