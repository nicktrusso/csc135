package hw1;

import java.io.*;
import java.util.Scanner;

public class Recognizer {
	static String inputString;
	static int index = 0;
	static int errorFlag = 0;
	
	private char token()
	{
		return inputString.charAt(index);
	}
	private void advancePtr()
	 { 
		 if (index < (inputString.length()-1)) 
			 index++; 
	 }
	 private void match(char T)
	 { 
		 if (T == token()) 
			 advancePtr(); 
		 else 
			 error(); 
	 }
	 private boolean isStatemt()
	 {
		 if(isLetter())
			 return true;  //start of asignmt()
		 else
			 if(isIfStmt())
				 return true;  //start of ifstmt()
			 else 
				 if(isforP()) //start of for()
					 return true;
				 else 
					 if(isInput())
						 return true; //start of input
					 else
						 if(isOutput())
							 return true; //start of output
						 else
							 return false;
	 }
	 private boolean isAsignmt()
	 {
		 return isLetter();
	 }
	 private boolean isIfStmt()
	 {
		 return (token() == 'I');
	 }
	 private boolean isforP()
	 {
		 return (token() == 'F');
	 }
	 private boolean isInput()
	 {
		 return (token() == 'N');
	 }
	 private boolean isOutput()
	 {
		 return (token() == 'O');
	 }
	 private boolean isDigit()
	 {
		 if((token() == '0') || (token() == '1') || (token() == '2') || (token() == '3') 
			|| (token() == '4') || (token() == '5') || (token() == '6') || (token() == '7'))
			return true;
		 else
			 return false;
	 }
	 private boolean isLetter()
	 {
		 if((token() == 'X' || token() == 'Y' || token() == 'Z'))
			 return true;
		 else
			 return false;
	 }
	 
	 private void error()
	 {
		 System.out.println("error at position: " + index);
		 errorFlag = 1;
		 advancePtr();
	 }
	 
	/////////////////////////////////////////////
	private void program()
	{
		do
		{
			statemt();
		}while(isStatemt());
		match('$');
	}
	private void statemt()
	{
		if(isAsignmt())
			asignmt();
		else
			if(isIfStmt())
				ifstmt();
			else
				if(isforP())
					forP();
				else
					if(isInput())
						input();
					else
						if(isOutput())
							output();
						else
							error();
	}
	private void asignmt()
	{
		ident();
		if(token() == '~')
		{
			match(token());
			exprsn();
			if(token() == ';')
				match(token());
			else
				error();
		}
		else
			error();
	}
	private void ifstmt()
	{
		if(isIfStmt())
		{
			match(token());
			comprsn();
			if(token() == '@')
			{
				match(token());
				if((isStatemt()) || (token() == '%'))
				{
					if(token() == '%')
						match(token());
					do
					{
						if(token() == '%')
							match(token());
						statemt();
					}while((isStatemt()) || (token() == '%'));
				}
				if(token() == '&')
				{
					match(token());
				}
				else error();
			}
			else error();
		}
		else error();
			
	}
	private void forP()
	{
		if(isforP())
		{
			match(token());
			if(token() == '(')
			{
				match(token());
				asignmt();
				if(token() == ')')
				{
					match(token());
					if(token() == '(')
					{
						match(token());
						comprsn();
						if(token() == ')')
						{
							match(token());
							if(token() == 'L')
							{
								match(token());
								if(isStatemt())
								{
									do
									{
										statemt();
									}while(isStatemt());
								}
								if(token() == '\\')
									match(token());
								else error();
							}
							else error();
						}
						else error();
					}
					else error();
				}
				else error();
			}
			else error();
		}
		else error();
	}
	private void input()
	{
		if(isInput())
		{
			match(token());
			ident();
			if(token() == ',')
			{
				do
				{
					match(token());
					ident();
				}while(token() == ',');
			}
			if(token() == ';')
				match(token());
			else
				error();
		}
		else
			error();
	}
	private void output()
	{
		if(isOutput())
		{
			match(token());
			ident();
			if(token() == ',')
			{
				do
				{
					match(token());
					ident();
				}while(token() == ',');
			}
			if(token() == ';')
				match(token());
			else
				error();
		}
		else
			error();
	}
	private void comprsn()
	{
		if(token() == '(')
		{
			match(token());
			oprnd();
			opratr();
			oprnd();
			if(token() == ')')
				match(token());
			else
				error();
		}
		else
			error();
	}
	private void exprsn()
	{
		factor();
		if((token() == '+') || (token() == '-')){
			do
			{
				sumop();
				factor();
			}while((token() == '+') || (token() == '-'));
		}
	}
	private void factor()
	{
		oprnd();
		if((token() == '*') || (token() == '/')){
			do
			{
				prodop();
				oprnd();
			}while((token() == '*') || (token() == '/'));
		}
	}
	private void sumop()
	{
		if((token() == '+') || (token() == '-'))
			match(token());
		else
			error();
	}
	private void prodop()
	{
		if((token() == '*') || (token() == '/'))
			match(token());
		else
			error();
	}
	private void oprnd()
	{
		if(isDigit())
			integer();
		else
			if(isLetter())
				ident();
			else
				if(token() == '(')
				{
					match(token());
					exprsn();
					if(token() == ')')
						match(token());
					else
						error();
				}
				else
					error();
	}
	private void opratr()
	{
		if((token() == '<') || (token() == '=') || (token() == '>') || (token() == '!'))
			match(token());
		else
			error();
	}
	private void ident()
	{
		letter();
		if(isLetter() || isDigit())
			do{
				charP();
			}while(isLetter() || isDigit());
	}
	private void charP()
	{
		if(isLetter())
			letter();
		else 
			if(isDigit()) 
				digit();
			else
				error();
	}
	private void integer()
	{
		do{
			digit();
		}while(isDigit());
	}
	private void letter()
	{
		if(isLetter())
			match(token());
		else
			error();
	}
	private void digit()
	{
		if (isDigit()) 
			match(token()); 
		else 
			error(); 
	}

	/////////////////////////////////////////////
	private void start()
	 {
		 program();
		 if (errorFlag == 0)
			 System.out.println("legal." + "\n");
		 else
			 System.out.println("errors found." + "\n");
	 }
	
	/////////////////////////////////////////////
	public static void main (String[] args) throws IOException
	 {
		 Recognizer rec = new Recognizer();
		 Scanner input = new Scanner(System.in);
		 System.out.print("\n" + "enter an expression: ");
		 inputString = input.nextLine();
		 rec.start();
		 input.close();
	 }
}
