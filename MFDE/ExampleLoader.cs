using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MFDE
{
    internal static class ExampleLoader
    {
        internal static string LoadExample(string exampleName)
        {
            switch (exampleName)
            {
                case "ListMap": return Properties.Resources.ListMap;
                case "TreeMap": return Properties.Resources.TreeMap;
                case "PartialApplication": return Properties.Resources.PartialApplication;
                case "Map": return Properties.Resources.ListMap;
                case "Fibonacci": return Properties.Resources.Fibonacci;
                case "List": return Properties.Resources.List;
                case "MaxProfitAlgo": return Properties.Resources.MaxProfitAlgo;
                case "SearchTree": return Properties.Resources.SearchTree;
                case "ListFold": return Properties.Resources.ListFold;
                case "Even": return Properties.Resources.Even;
                case "EvenTail": return Properties.Resources.EvenTail;
                    
                default: return string.Empty;
            }
        }
    }
}
