using FastColoredTextBoxNS;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MFDE
{
    class MFEditor : FastColoredTextBox
    {

        public MFEditor()
            : base()
        {
            TextChanged += Editor_TextChanged;
        }

        private void Editor_TextChanged(object sender, TextChangedEventArgs e)
        {
            e.ChangedRange.ClearStyle();
            e.ChangedRange.SetStyle(SyntaxHighlighter.MagentaStyle, @"\b(int|bool|list|tree)\b");
            e.ChangedRange.SetStyle(SyntaxHighlighter.BlueStyle, @"\b(fun|let|in|end|if|then|match|with|else|do)\b");
        }
    }
}
