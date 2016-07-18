using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MFDE
{
    public static class BuiltInFuncs
    {
        public static string LoadAll()
        {
            var printList = Properties.Resources.PrintList;
            var printTree = Properties.Resources.PrintTree;
            
            return printList + "\n" + printTree;
        }
    }
}
