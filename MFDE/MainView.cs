using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace MFDE
{
    public partial class mainViewForm : Form
    {

        private string selectedCompiler = "MicrosFSharpOld.exe";

        public mainViewForm()
        {
            InitializeComponent();
        }

        private string CompileProgram(string fileName)
        {
            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.FileName = selectedCompiler;
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.Arguments = fileName;
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            string error = p.StandardOutput.ReadToEnd();
            p.WaitForExit();
            return output;
        }

        private string RunProgram(string fileName)
        {
            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.FileName = fileName;
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            string error = p.StandardError.ReadToEnd();
            p.WaitForExit();
            return string.Format("{0} \n {1}", output, error);
        }

        private void CleanUp()
        {
            try
            {
                File.Delete("MyProgram.exe");
                File.Delete("SourceCode.txt");
            }
            catch (Exception)
            {

            }
        }

        private string SaveSourceCode(string mfSourceCode)
        {
            string sourceCodeFileName = "SourceCode.txt";
            File.WriteAllText(sourceCodeFileName, mfSourceCode);
            return sourceCodeFileName;
        }

        private void compileAndRunBtn_Click(object sender, EventArgs e)
        {
            outputRichTxtBx.Clear();
            CompileAndRun();
        }

        private void CompileAndRun()
        {
            if (!string.IsNullOrWhiteSpace(mfFastColoredEditor.Text))
            {
                var builtInFunsSourceCode = BuiltInFuncs.LoadAll();
                var sourceProgram = mfFastColoredEditor.Text;
                var programSourceCode = builtInFunsSourceCode + "\n" + sourceProgram;
                var sourceCodeFileName = SaveSourceCode(programSourceCode);
                if (File.Exists(sourceCodeFileName))
                {
                    outputRichTxtBx.Text = CompileProgram(sourceCodeFileName);
                    if (File.Exists("MyProgram.exe"))
                    {
                        GetRunProgramBgWrk().RunWorkerAsync();
                    }
                }
            }
        }

        private void ExampleBtn_Click(object sender, EventArgs e)
        {
            var exampleBtn = (Button)sender;
            var exampleName = exampleBtn.Tag.ToString();
            mfFastColoredEditor.Text = ExampleLoader.LoadExample(exampleName);
        }

        private void MainView_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.F5)
            {
                CompileAndRun();
            }
        }

        private void tailChkBtn_CheckedChanged(object sender, EventArgs e)
        {
            selectedCompiler = tailChkBtn.Checked ? "MicroFSharpTail.exe" : "MicrosFSharpOld.exe";
        }

        private BackgroundWorker GetRunProgramBgWrk()
        {
            var bgWrk = new BackgroundWorker();
            bgWrk.DoWork += BgWrk_DoWork;
            bgWrk.RunWorkerCompleted += BgWrk_RunWorkerCompleted;
            return bgWrk;
        }

        private void BgWrk_RunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
        {
            var result = e.Result as string;
            outputRichTxtBx.Text = result;
            CleanUp();
        }

        private void BgWrk_DoWork(object sender, DoWorkEventArgs e)
        {
            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = true;
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.FileName = "MyProgram";
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            string error = p.StandardError.ReadToEnd();
            p.WaitForExit();
            e.Result = string.Format("{0} \n {1}", output, error);
        }
    }
}
