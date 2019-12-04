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

    public partial class AddUserWindow : Form
    {
        public AddUserWindow()
        {
            InitializeComponent();                                 
            MainForm mainf = new MainForm();                        
        }

        private void buttonAdd_Click(object sender, EventArgs e)
        {
            string login, password;
            byte[] data = new byte[50];

            login = textBoxLogin.Text;
            password = textBoxPass.Text;


            Datagram addUser = new Datagram(Commands.ADD_USER, Datagram.PackLogPassData(login, password, data));
            addUser.Send(MainForm.socket);
            addUser.ReceiveData(MainForm.socket);

            if (addUser.data[0] == 0)
            {
                MessageBox.Show("User added");
            }               // to do refresh page with the new login and password 
        }
 

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }


    }
}
