using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ClientApp
{
    class TestJSON
    {
        public string text { get; set; }
        public string[] variants { get; set; }
    }

    class AnswersJSON
    {
        public byte [] answers { get; set; }
    }

    class PassedTest
    {
        public string pass { get; set; }
        public string all { get; set; }
    }

    class UserInfo
    {
        public string [] groups { get; set; }
        public string login { get; set; }
        public string name { get; set; }
        public string surname { get; set; } 
    }
}
