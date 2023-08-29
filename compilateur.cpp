//  A compiler from a very simple Pascal-like structured language LL(k)
//  to 64-bit 80x86 Assembly langage
//  Copyright (C) 2019 Pierre Jourlin
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.

// Build with "make compilateur"


#include <string>
#include <iostream>
#include <cstdlib>
#include <map>
#include <set>
#include <FlexLexer.h>
#include "tokeniser.h"
#include <cstring>

using namespace std;
enum TYPE{UNSIGNED_INT,BOOLEAN, INCONNU,INTEGER};
enum OPREL {EQU, DIFF, INF, SUP, INFE, SUPE, WTFR};
enum OPADD {ADD, SUB, OR, WTFA};
enum OPMUL {MUL, DIV, MOD, AND ,WTFM};

TOKEN current;				// Current token


FlexLexer* lexer = new yyFlexLexer; // This is the flex tokeniser
// tokens can be read using lexer->yylex()
// lexer->yylex() returns the type of the lexicon entry (see enum TOKEN in tokeniser.h)
// and lexer->YYText() returns the lexicon entry as a string

	
map<string, enum TYPE> DeclaredVariables;
unsigned long TagNumber=0;
unsigned long WhileNumber=0;
unsigned long ForNumber=0;
unsigned long CaseNumber=0;
unsigned long SwitchNumber=0;
bool IsDeclared(const char *id){
	return DeclaredVariables.find(id)!=DeclaredVariables.end();
}


void Error(string s){
	cerr << "Ligne n°"<<lexer->lineno()<<", lu : '"<<lexer->YYText()<<"'("<<current<<"), mais ";
	cerr<< s << endl;
	exit(-1);
}

// Program := [DeclarationPart] StatementPart
// DeclarationPart := "[" Letter {"," Letter} "]"
// StatementPart := Statement {";" Statement} "."
// AssignementStatement := Letter "=" Expression

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
// SimpleExpression := Term {AdditiveOperator Term}
// Term := Factor {MultiplicativeOperator Factor}
// Factor := Number | Letter | "(" Expression ")"| "!" Factor
// Number := Digit{Digit}

// AdditiveOperator := "+" | "-" | "||"
// MultiplicativeOperator := "*" | "/" | "%" | "&&"
// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
// Digit := "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9"
// Letter := "a"|...|"z"
//Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement ,
//IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]	,
//WhileStatement := "WHILE" Expression "DO" Statement	,
//ForStatement := "FOR" AssignementStatement "To" Expression "DO" Statement	,
//BlockStatement := "BEG" Statement { ";" Statement } "END"	,
//
		
TYPE Identifier(void){
	enum TYPE type;
	if(!IsDeclared(lexer->YYText())){
		cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<endl;
		exit(-1);
	}
	type=DeclaredVariables[lexer->YYText()];
	cout << "\tpush "<<lexer->YYText()<<endl;
	current=(TOKEN) lexer->yylex();
	return type;
}

TYPE Number(void){
	cout <<"\tpush $"<<atoi(lexer->YYText())<<endl;
	current=(TOKEN) lexer->yylex();
	return INTEGER;
}
TYPE Type(void){ // Le typage marche que pour le booléan et le Integer 
	if(current!=TYPAGE)
		Error("Le type des variables est attendu !");
	if(strcmp(lexer->YYText(),"BOOLEAN")==0){
		current=(TOKEN) lexer->yylex();
		return BOOLEAN;
	}	
	else if(strcmp(lexer->YYText(),"INTEGER")==0){
		current=(TOKEN) lexer->yylex();
		return INTEGER;
	}
	else if(strcmp(lexer->YYText(),"UNSIGNED_INT")==0){
		current=(TOKEN) lexer->yylex();
		return UNSIGNED_INT;
	}
	else
		Error("Le type donné est inconnu !");
		return INCONNU;
}
TYPE Expression(void);			// Called by Term() and calls Term()

TYPE Factor(void){
	if(current==RPARENT){
		current=(TOKEN) lexer->yylex();
		TYPE type=Expression();
		if(current!=LPARENT)
			Error("')' était attendu");		// ")" expected
		else
			current=(TOKEN) lexer->yylex();
			return type;
	}
	else 
		if (current==NUMBER){
			return Number();
		}	
	    else{
			if(current==ID)
				return Identifier();
			else
				Error("'(' ou chiffre ou lettre attendue");
				return INCONNU;
		}
			

}

// MultiplicativeOperator := "*" | "/" | "%" | "&&"
OPMUL MultiplicativeOperator(void){
	OPMUL opmul;
	if(strcmp(lexer->YYText(),"*")==0)
		opmul=MUL;
	else if(strcmp(lexer->YYText(),"/")==0)
		opmul=DIV;
	else if(strcmp(lexer->YYText(),"%")==0)
		opmul=MOD;
	else if(strcmp(lexer->YYText(),"&&")==0)
		opmul=AND;
	else opmul=WTFM;
	current=(TOKEN) lexer->yylex();
	return opmul;
}

// Term := Factor {MultiplicativeOperator Factor}
TYPE Term(void){
	OPMUL mulop;
	TYPE var1=Factor();
	while(current==MULOP){
		mulop=MultiplicativeOperator();		// Save operator in local variable
		TYPE var2=Factor();
		if(var1!=var2){
			Error("LES OPERANDE NE SONT PAS DE MEME TYPE !");
		}
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(mulop){
			case AND:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# AND"<<endl;	// store result
				break;
			case MUL:
				cout << "\tmulq	%rbx"<<endl;	// a * b -> %rdx:%rax
				cout << "\tpush %rax\t# MUL"<<endl;	// store result
				break;
			case DIV:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// quotient goes to %rax
				cout << "\tpush %rax\t# DIV"<<endl;		// store result
				break;
			case MOD:
				cout << "\tmovq $0, %rdx"<<endl; 	// Higher part of numerator  
				cout << "\tdiv %rbx"<<endl;			// remainder goes to %rdx
				cout << "\tpush %rdx\t# MOD"<<endl;		// store result
				break;
			default:
				Error("opérateur multiplicatif attendu");
		}
	}
	return var1;
}

// AdditiveOperator := "+" | "-" | "||"
OPADD AdditiveOperator(void){
	OPADD opadd;
	if(strcmp(lexer->YYText(),"+")==0)
		opadd=ADD;
	else if(strcmp(lexer->YYText(),"-")==0)
		opadd=SUB;
	else if(strcmp(lexer->YYText(),"||")==0)
		opadd=OR;
	else opadd=WTFA;
	current=(TOKEN) lexer->yylex();
	return opadd;
}

// SimpleExpression := Term {AdditiveOperator Term}
TYPE SimpleExpression(void){
	OPADD adop;
	TYPE term1=Term();
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		TYPE term2=Term();
		if(term1!=term2){
			Error("Les termes ne sont pas de même type !");
		}
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(adop){
			case OR:
				cout << "\taddq	%rbx, %rax\t# OR"<<endl;// operand1 OR operand2
				break;			
			case ADD:
				cout << "\taddq	%rbx, %rax\t# ADD"<<endl;	// add both operands
				break;			
			case SUB:	
				cout << "\tsubq	%rbx, %rax\t# SUB"<<endl;	// substract both operands
				break;
			default:
				Error("opérateur additif inconnu");
		}
		cout << "\tpush %rax"<<endl;			// store result
	}
	return term1;// je renvoie le  type des termes
}

//VarDeclaration := Ident {"," Ident} ":" Type
void VarDeclaration(void){
	set<string> idents;// Je stock tout les identificateurs de meme type,
	if(current!=ID)
		Error("Un identificater était attendu");
	idents.insert(lexer->YYText());
	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificateur était attendu");
		idents.insert(lexer->YYText());
		current=(TOKEN) lexer->yylex();
	}
	if(current!=COLON){
		Error("Deux points attendu !");
	}
	current=(TOKEN) lexer->yylex();
	TYPE type=Type();
	for (set<string>::iterator it=idents.begin(); it!=idents.end(); ++it){// pour tout les itérateurs stocké je met le type choisi
	    cout << *it << ":\t.quad 0"<<endl;
            DeclaredVariables[*it]=type;
	}
}

// VarDeclarationPart := "VAR" VarDeclaration {";" VarDeclaration} "."
void VarDeclarationPart(void){
	current=(TOKEN) lexer->yylex();
	VarDeclaration();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		VarDeclaration();
	}
	if(current!=DOT){
		Error("Point attendu a la fin de la déclaration !");
	}
	current=(TOKEN) lexer->yylex();
}


// DeclarationPart := "[" Ident {"," Ident} "]"
/*void DeclarationPart(void){
	if(current!=RBRACKET)
		Error("caractère '[' attendu");
	cout << "\t.data"<<endl;
	cout<<"FormatString1:\t.string \"\%llu\\n\"\t# used by printf to display 64-bit unsigned integers"<<endl;
	cout << "\t.align 8"<<endl;
	current=(TOKEN) lexer->yylex();
	if(current!=ID)
		Error("Un identificater était attendu");
	cout << lexer->YYText() << ":\t.quad 0"<<endl;
	DeclaredVariables.insert(lexer->YYText());
	current=(TOKEN) lexer->yylex();
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		if(current!=ID)
			Error("Un identificateur était attendu");
		cout << lexer->YYText() << ":\t.quad 0"<<endl;
		DeclaredVariables.insert(lexer->YYText());
		current=(TOKEN) lexer->yylex();
	}
	if(current!=LBRACKET)
		Error("caractère ']' attendu");
	current=(TOKEN) lexer->yylex();
}*/

// RelationalOperator := "==" | "!=" | "<" | ">" | "<=" | ">="  
OPREL RelationalOperator(void){
	OPREL oprel;
	if(strcmp(lexer->YYText(),"==")==0)
		oprel=EQU;
	else if(strcmp(lexer->YYText(),"!=")==0)
		oprel=DIFF;
	else if(strcmp(lexer->YYText(),"<")==0)
		oprel=INF;
	else if(strcmp(lexer->YYText(),">")==0)
		oprel=SUP;
	else if(strcmp(lexer->YYText(),"<=")==0)
		oprel=INFE;
	else if(strcmp(lexer->YYText(),">=")==0)
		oprel=SUPE;
	else oprel=WTFR;
	current=(TOKEN) lexer->yylex();
	return oprel;
}

// Expression := SimpleExpression [RelationalOperator SimpleExpression]
TYPE Expression(void){
	OPREL oprel;
	TYPE exp1=SimpleExpression();
	if(current==RELOP){
		oprel=RelationalOperator();
		TYPE exp2=SimpleExpression();
		if(exp1!=exp2){
			Error("LES EXPRESSIONS NE SONT PAS DE MÊME TYPE");
		}
		cout << "\tpop %rax"<<endl;
		cout << "\tpop %rbx"<<endl;
		cout << "\tcmpq %rax, %rbx"<<endl;
		switch(oprel){
			case EQU:
				cout << "\tje Vrai"<<++TagNumber<<"\t# If equal"<<endl;
				break;
			case DIFF:
				cout << "\tjne Vrai"<<++TagNumber<<"\t# If different"<<endl;
				break;
			case SUPE:
				cout << "\tjae Vrai"<<++TagNumber<<"\t# If above or equal"<<endl;
				break;
			case INFE:
				cout << "\tjbe Vrai"<<++TagNumber<<"\t# If below or equal"<<endl;
				break;
			case INF:
				cout << "\tjb Vrai"<<++TagNumber<<"\t# If below"<<endl;
				break;
			case SUP:
				cout << "\tja Vrai"<<++TagNumber<<"\t# If above"<<endl;
				break;
			default:
				Error("Opérateur de comparaison inconnu");
		}
		cout << "\tpush $0\t\t# False"<<endl;
		cout << "\tjmp Suite"<<TagNumber<<endl;
		cout << "Vrai"<<TagNumber<<":\tpush $0xFFFFFFFFFFFFFFFF\t\t# True"<<endl;	
		cout << "Suite"<<TagNumber<<":"<<endl;
		return BOOLEAN;
	}
	return exp1;

}
void Statement(void);
//IfStatement := "IF" Expression "THEN" Statement [ "ELSE" Statement ]	
void IfStatement(void){
	if(current!=IF){
		Error("IF ATTENDU");
	}
	cout<<"IF"<<TagNumber<<":"<<endl;
	current=(TOKEN) lexer->yylex();
	TYPE expr=Expression();
	if(expr!=BOOLEAN){
		Error("CONDITION INVALIDE DANS LE IF");
	}
	// Saut sur else si il existe
	// Mais avant on récupère en haut de la pile le résultat de la comparaison
	cout<<"\tpop %rax"<<endl;
	cout<<"\tcmpq $0, %rax"<<endl;
	cout << "\tjz Else"<<TagNumber-1 << endl;
	
	if(current!=THEN){
		Error("THEN ATTENDU");
	}
	current=(TOKEN) lexer->yylex();
	
	Statement();
	cout<<"\tjmp FinIf"<<TagNumber-1<<endl;

	//Etiquette else
	cout<<"Else"<<TagNumber-1<<":"<<endl;
	if(current==ELSE){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	//Etiquette fin if
	cout<<"FinIf"<<TagNumber-1<<":"<<endl;
}
// AssignementStatement := Identifier ":=" Expression
void AssignementStatement(void){
	string variable;
	if(current!=ID)
		Error("Identificateur attendu");
	if(!IsDeclared(lexer->YYText())){
		cerr << "Erreur : Variable '"<<lexer->YYText()<<"' non déclarée"<<endl;
		exit(-1);
	}
	variable=lexer->YYText();
	TYPE identifier=Identifier();
	if(current!=ASSIGN)
		Error("caractères ':=' attendus");
	current=(TOKEN) lexer->yylex();
	TYPE expr=Expression();
	if(identifier!=expr){
		Error("l'identifier et l'expression ne sont pas de même type!");
	}
	cout << "\tpop "<<variable<<endl;
}
//WhileStatement := "WHILE" Expression "DO" Statement, fonctionnel
void WhileStatement(void) {
    if (current != WHILE) {
        Error("while attendu !");
    }
    // étiquette de début de la boucle while
    cout << "DebutTantQue"<<WhileNumber <<":"<< endl;
    current = (TOKEN) lexer->yylex();
    TYPE expr=Expression();
	if(expr!=BOOLEAN){
		Error("CONDITION INVALIDE DANS LE WHILE !");
	}
    // utiliser la condition ZF pour sauter à la fin de la boucle
	cout<<"\tpop %rax"<<endl;
	cout<<"\tcmpq $0, %rax"<<endl;
    cout << "\tjz FinTantQue"<<WhileNumber << endl;
    if (current != DO) {
        Error("DO ATTENDU !");
    }
    current = (TOKEN) lexer->yylex();
    Statement();
    // sauter au début de la boucle while
    cout << "\tjmp DebutTantQue"<<WhileNumber << endl;
    // étiquette de fin de la boucle while
    cout << "FinTantQue"<<WhileNumber<<":" << endl;
	WhileNumber++;
}
//ForStatement := "FOR" AssignementStatement "to" Expression "DO" Statement, fonctionnel
void ForStatement(void) {
    if (current != FOR) {
        Error("For attendu !");
    }
    current = (TOKEN) lexer->yylex();
    AssignementStatement();
    // étiquette de début de la boucle for
    cout << "DebutPour"<<ForNumber<<":" << endl;
	if(current!=TO){
		Error("to attendu !");
	}
	current = (TOKEN) lexer->yylex();
    TYPE expr=Expression();
	if(expr!=BOOLEAN){
		Error("L'expression dans le for n'est pas de type BOOLEAN");
	}
    // utiliser la condition ZF pour sauter à la fin de la boucle
	cout<<"\tpop %rax"<<endl;
	cout<<"\tcmpq $0, %rax"<<endl;
    cout << "\tjz FinPour" <<ForNumber<< endl;
    if (current != DO) {
        Error("DO ATTENDU !");
    }
    current = (TOKEN) lexer->yylex();
    Statement();
    
    // sauter au début de la boucle for
    cout << "\tjmp DebutPour"<<ForNumber << endl;
    // étiquette de fin de la boucle for
    cout << "FinPour"<<ForNumber<<":" << endl;
	ForNumber++;
}

/*D'apres le site la grammaire de constant est celle ci :
<constant> ::= <unsigned number> | <sign> <unsigned number> | <constant identifier> | <sign> <constant identifier>*/
// Ce qui ressemble beaucoup a simple expression, on peut avoir une somme ou juste un nomnbre*
void Constant(void){
	OPADD adop;
	TYPE term1=Term();
	
	while(current==ADDOP){
		adop=AdditiveOperator();		// Save operator in local variable
		if(adop==OR){
			Error("OR Non fonctionnel dans le case !");
		}
		TYPE term2=Term();
		if(term1!=term2){
			Error("Les termes ne sont pas de même type !");
		}
		cout << "\tpop %rbx"<<endl;	// get first operand
		cout << "\tpop %rax"<<endl;	// get second operand
		switch(adop){
			case OR:
				cout << "\taddq	%rbx, %rax\t# OR"<<endl;// operand1 OR operand2
				break;			
			case ADD:
				cout << "\taddq	%rbx, %rax\t# ADD"<<endl;	// add both operands
				break;			
			case SUB:	
				cout << "\tsubq	%rbx, %rax\t# SUB"<<endl;	// substract both operands
				break;
			default:
				Error("opérateur additif inconnu");
		}
		cout << "\tpush %rax"<<endl;			// store result
	}
	
}
//<case label list> ::= <constant> {, <constant> }
void CaseLabelList(){
	Constant();
	cout << "\tpop %rax"<<endl;
	cout << "\tpop %rbx"<<endl;
	cout << "\tcmpq %rax, %rbx"<<endl;
	cout << "\tpush %rbx"<<endl;
	cout << "\tje Statement"<<SwitchNumber+1<<endl;
	while(current==COMMA){
		current=(TOKEN) lexer->yylex();
		Constant();
		cout << "\tpop %rax"<<endl;
		cout << "\tpop %rbx"<<endl;
		cout << "\tcmpq %rax, %rbx"<<endl;
		cout << "\tpush %rbx"<<endl;
		cout << "\tje Statement"<<SwitchNumber+1<<endl;
	}
	cout << "\tjne Switch"<<++SwitchNumber<<endl;
} 


//<case list element> ::= <case label list> : <statement> | <empty> 
void CaseListElement(){
	CaseLabelList();
	if(current!=COLON){
		Error("Deux point manquant dans une ligne du case !");
	}
	current=(TOKEN) lexer->yylex();
	cout<<"Statement"<<SwitchNumber<<":"<<endl;
	Statement();
	cout<<"\tjmp EndCase"<<CaseNumber<<endl;
	if(current!=SEMICOLON){
		Error("Un ; est attendu !");
	}
	current=(TOKEN) lexer->yylex();
	
	cout<< "Switch"<<SwitchNumber<<":"<<endl;
}
//case <expression> of <case list element> {; <case list element> } end 
void CaseStatement(){
	if(current!=CASE){
		Error("Case Manquant !");
	}
	cout<<"CASE"<<CaseNumber<<":"<<endl;
	current=(TOKEN) lexer->yylex();
	TYPE expr=Expression();
	if(current!=OF){
		Error("of manquant dans le case");
	}
	current=(TOKEN) lexer->yylex();
	cout<< "Switch"<<SwitchNumber<<":"<<endl;
	CaseListElement();
	while(current!=END){
		CaseListElement();
	}
	if(current!=END){
		Error("End manquant à la fin du case");
	}
	cout<<"EndCase"<<CaseNumber<<":"<<endl;
	CaseNumber++;
	current=(TOKEN) lexer->yylex();
}

//BlockStatement := "BEG" Statement { ";" Statement } "END"   
void BlockStatement(void){
	if(current !=BEG){
		Error("BEG attendu !");
	}
	current=(TOKEN) lexer->yylex();
	Statement();
	while(current == SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=END){
		Error("end ATTENDU !");
	}
	current=(TOKEN) lexer->yylex();
	
}

void DisplayStatement(){
	if(current!=DISPLAY){
		Error("DISPLAY ATTENDU !");
	}
	current=(TOKEN) lexer->yylex();
	TYPE expr=Expression();
	if(expr!=INTEGER){
		Error("UNSIGNED INT ATTENDU !");
	}
	cout<<"\tpop %rdx\t# The value to be displayed"<<endl;
	cout<<"\tmovq $FormatString1, %rsi\t# \%llu\\n"<<endl;
	cout<<"\tmovl $1, \%edi"<<endl;
	cout<<"\tmovl $0, \%eax"<<endl;
	cout<<"\tcall __printf_chk@PLT"<<endl;
}
//Statement := AssignementStatement | IfStatement | WhileStatement | ForStatement | BlockStatement , marche normalement
void Statement(void){
	if(current==DISPLAY){
		DisplayStatement();
	}
	else if (current==IF){
		IfStatement();
	}
	else if (current==FOR){
		ForStatement();
	}
	else if (current==BEG){
		BlockStatement();
	}
	else if (current==WHILE){
		WhileStatement();
	}
	else if(current==ID){
		AssignementStatement();
	}
	else if(current==CASE){
		CaseStatement();
	}
}

// StatementPart := Statement {";" Statement} "."
void StatementPart(void){
	cout << "\t.text\t\t# The following lines contain the program"<<endl;
	cout << "\t.globl main\t# The main function must be visible from outside"<<endl;
	cout << "main:\t\t\t# The main function body :"<<endl;
	cout << "\tmovq %rsp, %rbp\t# Save the position of the stack's top"<<endl;
	Statement();
	while(current==SEMICOLON){
		current=(TOKEN) lexer->yylex();
		Statement();
	}
	if(current!=DOT)
		Error("caractère '.' attendu");
	current=(TOKEN) lexer->yylex();
}

// Program := [DeclarationPart] StatementPart
void Program(void){
	if(current==VAR)
		VarDeclarationPart();
	StatementPart();	
}

int main(void){	// First version : Source code on standard input and assembly code on standard output
	// Header for gcc assembler / linker
	cout << "\t\t\t# This code was produced by the CERI Compiler"<<endl;
	cout << "\t.data"<<endl;
	cout<<"FormatString1:\t.string \"\%llu\\n\"\t# used by printf to display 64-bit unsigned integers"<<endl;
	cout << "TrueString:\t.string \"TRUE\\n\"\t# used by printf to display the boolean value TRUE"<<endl; 
	cout << "FalseString:\t.string \"FALSE\\n\"\t# used by printf to display the boolean value FALSE"<<endl; 
	// Let's proceed to the analysis and code production
	current=(TOKEN) lexer->yylex();
	Program();
	// Trailer for the gcc assembler / linker
	cout << "\tmovq %rbp, %rsp\t\t# Restore the position of the stack's top"<<endl;
	cout << "\tret\t\t\t# Return from main function"<<endl;
	if(current!=FEOF){
		cerr <<"Caractères en trop à la fin du programme : ["<<current<<"]";
		Error("."); // unexpected characters at the end of program
	}

}