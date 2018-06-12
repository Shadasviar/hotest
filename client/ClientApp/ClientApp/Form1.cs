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
        public byte[] arrayOfAnswers;
        byte i = 0;
        public Dictionary<int, int> dicAnswers = new Dictionary<int, int>();
        public Dictionary<byte, string> dicGroups = new Dictionary<byte, string>();

        public delegate void delPassData(Dictionary<byte, string> dic);
        public delPassData delPass;     // this delegates will be used for add groups in user info

        public MainForm()
        {
            InitializeComponent();
            sendTestToolStripMenuItem.Enabled = false;

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

        private void addGroupToolStripMenuItem_Click(object sender, EventArgs e)// otpravlyat' tolko yesli nazhata klavisha ok
        {
            string groupName;

            groupName = Interaction.InputBox("Group name","Input group name");
            byte[] groupN = new byte[groupName.Length];
            dicGroups.Add(i,groupName);
            i++;
            

            Datagram addGroupReq = new Datagram(Commands.ADD_GROUP,Datagram.StringToByte(groupName, groupN));

            addGroupReq.Send(socket);
            addGroupReq.ReceiveData(socket);

            if (addGroupReq.data[0] == 0)
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

        private void getTestSizeToolStripMenuItem_Click(object sender, EventArgs e)
        {
            byte[] sendData = new byte[0];
            byte[] data = new byte[10];

            Datagram getSize = new Datagram(Commands.GET_TEST_LIST_SIZE, sendData);
            getSize.SendSize(socket, sendData);
            getSize.ReceiveData(socket);

            sizeOfListTest = data[0];

            if (getSize.data[0] > 0)
            {
                MessageBox.Show("Received {0} bytes", data[1].ToString());
            }
        }

        private void getTestToolStripMenuItem_Click(object sender, EventArgs e) // czy poprawnie? Jak lepiej tworzyc testy na formatce dynamicznie?
        {
            //sozdaem na forme testy

            getTestSizeToolStripMenuItem.PerformClick();        // dla otrzyamania size of list tests
            sendTestToolStripMenuItem.Enabled = true;          //sizeOfListTest        paramettr dla zliczania testow          

            byte[] tests = new byte[50];
            tests[0] = 1;
            string strResultJson = String.Empty;
            arrayOfAnswers = new byte[sizeOfListTest];

            Datagram getTest = new Datagram(Commands.GET_TEST, tests);

            Label labelQuestion = new Label();
            RadioButton variant1;
            RadioButton variant2;
            RadioButton variant3;
            RadioButton variant4;
            Button buttonSendAnswers = new Button();
            buttonSendAnswers.Text = "Send";

            for (int i = 0; i < sizeOfListTest; ++i)
            {
             
                getTest.Send(socket);
                getTest.ReceiveData(socket);
                strResultJson = System.Text.Encoding.UTF8.GetString(getTest.data.ToArray()); //pomeshchaetsya li ves vopros s otvetami?
                TestJSON oneTest = new TestJSON();

                int n = 0;
                labelQuestion = new Label();
                variant1 = new RadioButton();
                variant2 = new RadioButton();
                variant3 = new RadioButton();
                variant4 = new RadioButton();


                labelQuestion.Location = new Point(20, 40 + n);

                variant1.Location = new Point(labelQuestion.Location.X + 20, labelQuestion.Location.Y + 20);
                variant2.Location = new Point(labelQuestion.Location.X + 20, labelQuestion.Location.Y + 40);
                variant3.Location = new Point(labelQuestion.Location.X + 20, labelQuestion.Location.Y + 60);
                variant4.Location = new Point(labelQuestion.Location.X + 20, labelQuestion.Location.Y + 80);
                n += 120;

                try
                {
                    //tut parsim stroku
                    oneTest = JsonConvert.DeserializeObject<TestJSON>(strResultJson); // nado rasparsit
                    labelQuestion.Text = oneTest.text;
                    variant1.Text = oneTest.variants[0];
                    variant2.Text = oneTest.variants[1];
                    variant3.Text = oneTest.variants[2];
                    variant4.Text = oneTest.variants[3];

                    if (variant1.Checked == true) dicAnswers.Add(i,1);
                    else if (variant2.Checked == true) dicAnswers.Add(i, 2);
                    else if (variant3.Checked == true) dicAnswers.Add(i, 3);
                    else if (variant4.Checked == true) dicAnswers.Add(i, 4);


                }
                catch(Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            
            
        }
        string MyDictionaryToJson(Dictionary<int, int> dict)
        {
            var entries = dict.Select(d =>
                string.Format("\"{0}\":\"{1}\"", d.Key, string.Join(", ", d.Value)));
            return "{\"answers\":[{" + string.Join("}, {", entries) +"}" + "]}";
        }


        private void sendTestToolStripMenuItem_Click(object sender, EventArgs e)
        {

            byte[] getResults = new byte[0];
            string json = MyDictionaryToJson(dicAnswers);
           
            byte[] arrToSend = System.Text.Encoding.UTF8.GetBytes(json);
            Datagram sendAnswers = new Datagram(Commands.SEND_TEST_ANSWERS,arrToSend);

            sendAnswers.Send(socket);
            sendAnswers.ReceiveData(socket);

            Datagram getAnswers = new Datagram(Commands.SEND_TEST_ANSWERS, getResults);

            getAnswers.Send(socket);
            sendAnswers.ReceiveData(socket);

            json = System.Text.Encoding.UTF8.GetString(sendAnswers.data.ToArray());//messagebox w kotorom budet skolko so skolki pravilno
            string pass, all;

            PassedTest passedT = new PassedTest();
            passedT = JsonConvert.DeserializeObject<PassedTest>(json);
            pass = passedT.pass;
            all = passedT.all;

            MessageBox.Show(pass+"/"+all);
        }

        private void addUserToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AddUserWindow form = new AddUserWindow();
                    
            form.Show();
        }

        private void removeUserToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string userName = Interaction.InputBox("User name", "Input user name you want to remove");


            byte[] login = new byte[userName.Length];
            Datagram removeUser = new Datagram(Commands.DELETE_USER, Datagram.StringToByte(userName,login));

            removeUser.Send(socket);
            removeUser.ReceiveData(socket);

            if(removeUser.data[0] == 0)
            {
                MessageBox.Show("User has been removed");
            }
        }

        private void removeGroupToolStripMenuItem_Click(object sender, EventArgs e)
        {
            string groupName = Interaction.InputBox("Group name", "Input group name you want to remove");

            byte[] group = new byte[groupName.Length];

            Datagram removeGroup = new Datagram(Commands.DELETE_GROUP, Datagram.StringToByte(groupName,group));

            removeGroup.Send(socket);
            removeGroup.ReceiveData(socket);

            if (removeGroup.data[0] == 0)
            {
                MessageBox.Show("Group has been removed");
            }
        }

        private void buttonUserInfo_Click(object sender, EventArgs e)
        {
            

            UserInfoWindow form = new UserInfoWindow();

            form.Show();
        }
    }
}
