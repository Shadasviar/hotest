using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace ClientApp
{
    public partial class LogInWindow : Form
    {
        string adress = "192.168.0.157";
        int port = 6666;
        public static string login;
        public static string password;

        public LogInWindow()
        {
            InitializeComponent();

            IPEndPoint ipPoint = new IPEndPoint(IPAddress.Parse(adress), port);

            try
            {
                MainForm.socket.Connect(ipPoint);
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
        }
       
        

        private void buttonLogin_Click(object sender, EventArgs e)
        {

            login = textBoxLogin.Text;
            password = textBoxPassword.Text;
            byte[] bytes = new byte[50];

            Datagram message = new Datagram(Commands.OPEN_SESSION, Datagram.PackLogPassData(login, password, bytes));

            message.Send(MainForm.socket);
            message.ReceiveData(MainForm.socket);

            if (message.data[0] == 0)
            {
                MessageBox.Show("Success");
                MainForm form1 = new MainForm();
                form1.Show();
                this.Hide();
            }
            else
            {
                MessageBox.Show("Try again");
                MainForm.socket.Shutdown(SocketShutdown.Both); //use it for end program
                MainForm.socket.Close();
                Application.Exit();
            }
        }

        private void LogInWindow_KeyDown(object sender, KeyEventArgs e)
        {
            if(e.KeyCode == Keys.Enter)
            {
                buttonLogin.PerformClick();
            }
        }
    }
}
