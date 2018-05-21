namespace ClientApp
{
    partial class ChangeLogPass
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.buttonApply = new System.Windows.Forms.Button();
            this.labellogin = new System.Windows.Forms.Label();
            this.labelpass = new System.Windows.Forms.Label();
            this.textBoxLogin = new System.Windows.Forms.TextBox();
            this.textBoxPassword = new System.Windows.Forms.TextBox();
            this.labelNewLogin = new System.Windows.Forms.Label();
            this.labelNewPass = new System.Windows.Forms.Label();
            this.checkBoxLogin = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // buttonApply
            // 
            this.buttonApply.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.buttonApply.Location = new System.Drawing.Point(310, 157);
            this.buttonApply.Name = "buttonApply";
            this.buttonApply.Size = new System.Drawing.Size(75, 23);
            this.buttonApply.TabIndex = 0;
            this.buttonApply.Text = "Apply";
            this.buttonApply.UseVisualStyleBackColor = true;
            this.buttonApply.Click += new System.EventHandler(this.buttonApply_Click);
            // 
            // labellogin
            // 
            this.labellogin.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.labellogin.AutoSize = true;
            this.labellogin.Location = new System.Drawing.Point(236, 9);
            this.labellogin.Name = "labellogin";
            this.labellogin.Size = new System.Drawing.Size(66, 13);
            this.labellogin.TabIndex = 1;
            this.labellogin.Text = "Current login";
            // 
            // labelpass
            // 
            this.labelpass.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.labelpass.AutoSize = true;
            this.labelpass.Location = new System.Drawing.Point(237, 26);
            this.labelpass.Name = "labelpass";
            this.labelpass.Size = new System.Drawing.Size(89, 13);
            this.labelpass.TabIndex = 2;
            this.labelpass.Text = "Current password";
            // 
            // textBoxLogin
            // 
            this.textBoxLogin.Location = new System.Drawing.Point(95, 75);
            this.textBoxLogin.Name = "textBoxLogin";
            this.textBoxLogin.Size = new System.Drawing.Size(100, 20);
            this.textBoxLogin.TabIndex = 3;
            // 
            // textBoxPassword
            // 
            this.textBoxPassword.Location = new System.Drawing.Point(95, 114);
            this.textBoxPassword.Name = "textBoxPassword";
            this.textBoxPassword.Size = new System.Drawing.Size(100, 20);
            this.textBoxPassword.TabIndex = 4;
            // 
            // labelNewLogin
            // 
            this.labelNewLogin.AutoSize = true;
            this.labelNewLogin.Location = new System.Drawing.Point(33, 75);
            this.labelNewLogin.Name = "labelNewLogin";
            this.labelNewLogin.Size = new System.Drawing.Size(54, 13);
            this.labelNewLogin.TabIndex = 5;
            this.labelNewLogin.Text = "New login";
            // 
            // labelNewPass
            // 
            this.labelNewPass.AutoSize = true;
            this.labelNewPass.Location = new System.Drawing.Point(10, 114);
            this.labelNewPass.Name = "labelNewPass";
            this.labelNewPass.Size = new System.Drawing.Size(77, 13);
            this.labelNewPass.TabIndex = 6;
            this.labelNewPass.Text = "New password";
            // 
            // checkBoxLogin
            // 
            this.checkBoxLogin.AutoSize = true;
            this.checkBoxLogin.Checked = true;
            this.checkBoxLogin.CheckState = System.Windows.Forms.CheckState.Checked;
            this.checkBoxLogin.Location = new System.Drawing.Point(202, 77);
            this.checkBoxLogin.Name = "checkBoxLogin";
            this.checkBoxLogin.Size = new System.Drawing.Size(15, 14);
            this.checkBoxLogin.TabIndex = 7;
            this.checkBoxLogin.UseVisualStyleBackColor = true;
            this.checkBoxLogin.CheckedChanged += new System.EventHandler(this.checkBoxLogin_CheckedChanged);
            // 
            // ChangeLogPass
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(397, 192);
            this.Controls.Add(this.checkBoxLogin);
            this.Controls.Add(this.labelNewPass);
            this.Controls.Add(this.labelNewLogin);
            this.Controls.Add(this.textBoxPassword);
            this.Controls.Add(this.textBoxLogin);
            this.Controls.Add(this.labelpass);
            this.Controls.Add(this.labellogin);
            this.Controls.Add(this.buttonApply);
            this.Name = "ChangeLogPass";
            this.Text = "Change login or password";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonApply;
        private System.Windows.Forms.Label labellogin;
        private System.Windows.Forms.Label labelpass;
        private System.Windows.Forms.TextBox textBoxLogin;
        private System.Windows.Forms.TextBox textBoxPassword;
        private System.Windows.Forms.Label labelNewLogin;
        private System.Windows.Forms.Label labelNewPass;
        private System.Windows.Forms.CheckBox checkBoxLogin;
    }
}