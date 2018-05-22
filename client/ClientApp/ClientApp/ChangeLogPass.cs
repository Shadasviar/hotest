using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace ClientApp
{
    public partial class ChangeLogPass : Form
    {
        public ChangeLogPass()
        {
            InitializeComponent();
            labellogin.Text += ": " + LogInWindow.login;
            labelpass.Text += ": " + LogInWindow.password;

                       
        }

        private void checkBoxLogin_CheckedChanged(object sender, EventArgs e)
        {
            //checkBoxLogin.Checked = true;
            if (checkBoxLogin.Checked == false) textBoxLogin.Enabled = false;
            else if (checkBoxLogin.Checked == true) textBoxLogin.Enabled = true;
        }

        private void buttonApply_Click(object sender, EventArgs e)
        private void buttonApply_Click(object sender, EventArgs e)          // zrobić możliwośc wprowadznia loginu i hasla tylko do 16 symbolow 
        {
            string newLogin, newPassword;
            byte[] data = new byte[50];

            if(checkBoxLogin.Checked == false)
            {
                newLogin = LogInWindow.login;
            }
            else
            {
                newLogin = textBoxLogin.Text;
            }
            newPassword = textBoxPassword.Text;
            
            Datagram chLogPassReq = new Datagram(Commands.CHANGE_CREDENTIALS,Datagram.PackLogPassData(newLogin,newPassword,data));
            chLogPassReq.Send(MainForm.socket);
            chLogPassReq.ReceiveData(MainForm.socket);

            if (chLogPassReq.data[1] == 7)
            {
                MessageBox.Show("Data have been successfully changed");
            }               // to do refresh page with the new login and password 
        }
    }
}
