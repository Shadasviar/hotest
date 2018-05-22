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
using Microsoft.VisualBasic;
using Newtonsoft.Json;


namespace ClientApp
{
    public partial class MainForm : Form
    {
        public static Socket socket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        public int sizeOfListTest = 0;

        public MainForm()
        {
            InitializeComponent();
           



           // socket.Shutdown(SocketShutdown.Both); //use it for end program
           // socket.Close();

        }

        private void closeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            byte[] data = new byte[20];

            Datagram closeRequest = new Datagram(Commands.CLOSE_SESSION, data);

            closeRequest.Send(socket);
            closeRequest.ReceiveData(socket);

            if (closeRequest.data[1] == 5)
            {
                socket.Shutdown(SocketShutdown.Both); //use it for end program
                socket.Close();
               // this.Close();
                LogInWindow.ActiveForm.Dispose();
                Application.Exit();
            }
        }

        private void addGroupToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string groupName;
            
            groupName = Interaction.InputBox("Group name","Input group name");
            byte[] groupN = new byte[groupName.Length];
            Datagram addGroupReq = new Datagram(Commands.ADD_GROUP,Datagram.StringToByte(groupName, groupN));

            addGroupReq.Send(socket);
            addGroupReq.ReceiveData(socket);

            if (addGroupReq.data[1] == 8)
            {
                MessageBox.Show("Group has been created"); //proverit' rabotaet li parvilno
            }

        }

        private void changeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            ChangeLogPass form = new ChangeLogPass();
            form.Show();
        }

        private void Form1_FormClosing(object sender, FormClosingEventArgs e)
        {
            if (MessageBox.Show("Are you sure?", "Message", MessageBoxButtons.YesNo) == System.Windows.Forms.DialogResult.No)
            {
                e.Cancel = true;
            }
            else
            {
                e.Cancel = false;

                closeToolStripMenuItem.PerformClick();
            }

        }

        private void getTestSizeToolStripMenuItem_Click(object sender, EventArgs e)         //sprawdzic
        {
            byte[] sendData = new byte[0];
            byte[] data = new byte[10];
            
            Datagram getSize = new Datagram(Commands.GET_TEST_LIST_SIZE, sendData);
            getSize.SendSize(socket, sendData);
            getSize.ReceiveData(socket);

            sizeOfListTest = data[0];

            if(getSize.data[0] > 0)
            {
                MessageBox.Show("Received {0} bytes", data[1].ToString());
            }
        }

        private void getTestToolStripMenuItem_Click(object sender, EventArgs e)     //zrobic mozliwosc wpowadzenia numera testu w oknie
        {
            getTestSizeToolStripMenuItem.PerformClick();        // dla otrzyamania size of list tests
            //sizeOfListTest        paramettr dla zliczania testow
                         
            byte[] tests = new byte[50];
            tests[0] = 1;
            string strResultJson = String.Empty;

            Datagram getTest = new Datagram(Commands.GET_TEST, tests);

            getTest.Send(socket);
            getTest.ReceiveDataJSON(socket, sizeOfListTest);

        }
    }
}
