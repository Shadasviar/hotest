﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;

namespace ClientApp
{
    public enum Commands
    {
        INVALID_COMMAND = 0,    // done
        GET_TEST_LIST_SIZE = 1,
        GET_TEST = 2,
        SEND_TEST_ANSWERS = 3,
        OPEN_SESSION = 4,       //done
        CLOSE_SESSION = 5,      //done
        GET_RESULTS = 6,
        CHANGE_CREDENTIALS = 7,
        ADD_GROUP = 8,          //done
        ADD_USER = 9,
        ERROR_DATAGRAM = 10,
        DELETE_USER = 11,
        DELETE_GROUP = 12,
        GET_USER_INFO = 13,
        SET_USER_INFO = 14,
        ADD_TO_GROUP = 15,
        REMOVE_FROM_GROUP = 16
    }

    class Datagram
    {
        public Commands command;
        public List<byte> data = new List<byte>();
        public char [] jdata;
        public TestJSON testJSON;

        public Datagram(Commands command, List<byte> data)
        {
            this.command = command;
            this.data = data;
        }

        public Datagram(Commands command, byte[] data)
        {
            this.command = command;
            this.data = new List<byte>(data);
        }


        public void ReceiveData(Socket socket)
        {
            byte[] smallBuffer = new byte[1];
            int count = 0;
            data.Clear();

            socket.Receive(smallBuffer);
            command = (Commands)smallBuffer[0];
            socket.Receive(smallBuffer);

            count = 0;
            do
            {
                byte[] bufferReceive = new byte[1];

                count += socket.Receive(bufferReceive);
                this.data.AddRange(bufferReceive);

            } while (count < smallBuffer[0]);

        }

        public static byte[] PackLogPassData(string login, string pass, byte[] output)
        {
            for (int i = 0; i < login.Length; i++)
            {
                output[i] = (byte)login[i];
            }
            for (int i = 0; i < pass.Length; i++)
            {
                output[20 + i] = (byte)pass[i];
            }
            return output;
        }

        public static byte[] PackLogPassGroup(string login, string pass, byte [] group, byte[] output)
        {
            for (int i = 0; i < login.Length; i++)
            {
                output[i] = (byte)login[i];
            }
            for (int i = 0; i < pass.Length; i++)
            {
                output[20 + i] = (byte)pass[i];
            }

            output[50] = group[0];
            return output;
        }

        public static byte[] StringToByte(string word, byte[] output)
        {
            for (int i = 0; i < word.Length; i++)
            {
                output[i] = (byte)word[i];
            }
            return output;
        }

        public void Send(Socket socket)
        {
            List<byte> frame = new List<byte>();
            int size = data.Capacity;

            frame.Add((byte)command);
            frame.Add((byte)size);

            for (int i = 0; i < size; ++i)
            {
                frame.Add(data[i]);
            }
            socket.Send(frame.ToArray());
        }

        public void SendSize(Socket socket, byte[] data)
        {
            List<byte> frame = new List<byte>();
            int size = data.Length;

            frame.Add((byte)command);
            frame.Add((byte)size);

            socket.Send(frame.ToArray());
        }




    }
}
