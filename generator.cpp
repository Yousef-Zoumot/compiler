/*
 * File:	generator.cpp
 *
 * Description:	This file contains the public and member function
 *		definitions for the code generator for Simple C.
 *
 *		Extra functionality:
 *		- putting all the global declarations at the end
 */

# include <sstream>
# include <iostream>
# include "generator.h"
# include "Register.h"
# include "machine.h"
# include "Tree.h"
# include "label.h"

using namespace std;


/* This needs to be set to zero if temporaries are placed on the stack. */

# define SIMPLE_PROLOGUE 1


/* Okay, I admit it ... these are lame, but they work. */

# define isNumber(expr)		(expr->_operand[0] == '$')
# define isRegister(expr)	(expr->_register != nullptr)
# define isMemory(expr)		(!isNumber(expr) && !isRegister(expr))


/* The registers that we are using in the assignment. */

static Register *rax = new Register("%rax", "%eax", "%al");
static Register *rdi = new Register("%rdi", "%edi", "%dil");
static Register *rsi = new Register("%rsi", "%esi", "%sil");
static Register *rdx = new Register("%rdx", "%edx", "%dl");
static Register *rcx = new Register("%rcx", "%ecx", "%cl");
static Register *r8 = new Register("%r8", "%r8d", "%r8b");
static Register *r9 = new Register("%r9", "%r9d", "%r9b");
static Register *parameters[] = {rdi, rsi, rdx, rcx, r8, r9};

// static Register *rbx = new Register("%rbx", "%ebx", "%bl");
// static Register *rbp = new Register("%rbp", "%ebp", "%bpl");
// static Register *rsp = new Register("%rsp", "%esp", "%spl");
//
static Register *r10 = new Register("%r10", "%r10d", "%r10b");
static Register *r11 = new Register("%r11", "%r11d", "%r11b");

// static Register *r12 = new Register("%r12", "%r12d", "%r12b");
// static Register *r13 = new Register("%r13", "%r13d", "%r13b");
// static Register *r14 = new Register("%r14", "%r14d", "%r14b");
// static Register *r15 = new Register("%r15", "%r15d", "%r15b");

// vector<Register *> registers = {r15, r14, r13, r12, r11, r10, r9, r8, rsp, rbp, rcx, rdx, rsi, rdi, rax};

vector<Register *> registers = {r11, r10, r9, r8, rcx, rdx, rsi, rdi, rax};

int temp_offset;

void assignTemp(Expression *expr)
{
  stringstream ss;
  temp_offset -= expr->type().size();
  ss << temp_offset << "(%rbp)";
  expr->_operand == ss.str();
}

void assign(Expression *expr, Register *reg)
{
  if(expr != nullptr){
    if(expr->_register != nullptr)
      expr->_register->_node = nullptr;

    expr->_register = reg;
  }

  if(reg != nullptr){
    if(reg->_node != nullptr)
      reg->_node->_register = nullptr;

    reg->_node = expr;
  }
}

void load(Expression *expr, Register *reg)
{
  if(reg->_node != expr){
    // assert(reg->_node == nullptr);

    unsigned size = expr->type().size();

    if(reg->_node != nullptr){
      assignTemp(reg->_node);
      cout << "\tmov\t" << reg->name(size);
      cout << ", " << reg->_node->_operand;
      cout << "\t# spill" << endl;
    }

    if (expr != nullptr){
      unsigned size = expr->type().size();
      cout << "\tmov\t" << expr << ", ";
      cout << reg->name(size) << endl;
    }
    assign(expr, reg);
  }
}

Register * getreg()
{
  for(unsigned i = 0; i < registers.size(); i++)
    if(registers[i]->_node == nullptr)
      return registers[i];

  load(nullptr, registers[0]);
  return registers[0];
}

void Expression::generate(){

	generate();
}

void Expression::generate(bool &indirect){
	indirect = false;
	generate();
}
void Dereference::generate(bool &indirect){
	indirect = true;
	_expr->generate();
	_operand = _expr->_operand;
}

void Expression::test(const Label &label, bool ifTrue)
{
  generate();

  if(_register == nullptr)
    load(this, getreg());

  cout << "\tcmp\t$0, " << this << endl;
  cout << (ifTrue ? "\tjne\t" : "\tje\t") << label << endl;

  assign(this, nullptr);
}

void release()
{
  for(unsigned i = 0; i < registers.size(); i++)
    assign(nullptr, registers[i]);
}

// for(unsigned i = 0; i < _args.size(); i++)
//   _args[i]->generate();
// for(i = 0; i < registers.size(); i++)
//   load(nullptr, registers[i]);

//Lecture #19
 //use cltd not cltq
 //equivilant to
// cltd
//  movl %eax, %edx
//  sarl $31, %edx
// cqto
//   movq %rax, %rdx
//   sarq %31, %rdx


// void LessThan::test(const Label &label, bool onTrue)
// {
//   _left->generate();
//   _right->generate();
//
//   if(_left->_register == nullptr)
//     load(_left, getreg());
//
//   cout << "\tcmp\t" << _right << ", " << _left << endl;
//   cout << (onTrue ? "\tjl\t" : "\tjge\t") << label << endl;
//
//   assign(_left, nullptr);
//   assign(_right, nullptr);
// }

// void LessThan::generate()
// {
//   _left->generate();
//   _right->generate();
//
//   cout << "\tmov\t" << _left << ", %rax" << endl;
// 	cout << "\tcomp\t" << _right << ", %rax" << endl;
// 	cout << "\tsetl\t" << "%al" << endl;
// 	cout << "\tmovzbl\t %al, %eax" << endl;
// 	cout << "\tmov\t %eax, " << ? << endl;
// }





void compare(Expression * _left, Expression * _right, Expression * this_thing, const std::string &operation){
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  cout << "\tcmp\t" << _right << ", " << _left << endl;
  cout << "\t" << operation << "\t" << _left->_register->name(1) << endl; // set variable ? shouldn't the name be 4?
  cout << "\tmovzbl\t" << _left->_register->name(1) << ", " << _left->_register->name(4) << endl;

  assign(_right, nullptr);
  assign(this_thing, _left->_register);
}

void Equal::generate()
{
  compare(_left, _right, this, "sete");
}

void NotEqual::generate()
{
  compare(_left, _right, this, "setne");
}

void LessThan::generate()
{
  compare(_left, _right, this, "setl");
}

void GreaterThan::generate()
{
  compare(_left, _right, this, "setg");
}

void LessOrEqual::generate()
{
  compare(_left, _right, this, "setle");
}

void GreaterOrEqual::generate()
{
  compare(_left, _right, this, "setge");
}


void Add::generate()
{
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  cout << "\tadd\t" << _right << ", " << _left << endl;

  assign(_right, nullptr);
  assign(this, _left->_register);
}

void Subtract::generate()
{
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  cout << "\tsub\t" << _right << ", " << _left << endl;

  assign(_right, nullptr);
  assign(this, _left->_register);
}

void Multiply::generate()
{
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  cout << "\timul\t" << _right << ", " << _left << endl;

  assign(_right, nullptr);
  assign(this, _left->_register);
}
// cout << "\tmov\t" << _left << ", %rax" << endl;
// cout << "\tmov\t" << _right << ", %rcx" << endl;
// cout << "\tcltd" << endl;
// cout << "\tidiv\t %rcx" << endl;
// cout << "\tmov\t %rax, " << this << endl;

void Divide::generate()
{
  _left->generate();
  _right->generate();

  load(_left, rax);
  load(nullptr, rdx);

  if(_right->_register == nullptr)
    load(_right, getreg());

  cout << "\tidiv\t" << _right << endl;

  assign(_right, nullptr);
  assign(this, _left->_register);
}

void Remainder::generate()
{
  _left->generate();
  _right->generate();

  load(_left, rax);
  load(nullptr, rdx);

  if(_right->_register == nullptr)
    load(_right, getreg());

  cout << "\tidiv\t" << _right << endl;

  assign(_right, nullptr);
  assign(this, rdx);
}

void Negate::generate()
{
  // cout << "\tmovl\t" << _expr << ", %eax" << endl;
  // cout << "\tnegl\t" << "%eax" << endl;
  // cout << "\taddl\t" << ? << endl;

  _expr->generate();

  if(_expr->_register == nullptr)
    load(_expr, getreg());

  cout << "\tneg\t" << _expr << endl;

  assign(this, _expr->_register);



}

void Not::generate()
{
  // cout << "\tmovl\t" << _expr << ", %eax" << endl;
  // cout << "\tcmpl\t" << "$0, %eax" << endl;
  // cout << "\tsete %al" << endl;
  // cout << "\tmovzbl %al, %eax" << endl;

  _expr->generate();

  if(_expr->_register == nullptr)
    load(_expr, getreg());

  cout << "\tcmpl\t$0, " << _expr << endl;
  cout << "\tsete\t%" << _expr->_register->name(1) << endl;
  cout << "\tmovzbl\t%" << _expr->_register->name(1) << ", %" << _expr->_register->name(4) << endl;

  assign(this, _expr->_register);

}

void Dereference::generate()
{
  // cout <<"\tmovl\t" << _expr << ", %rax" << endl;
  // cout << (_type.size() == 1 ? "\tmovsbl\t" : "\tmovl\t") << "\t(%rax), %rax" << endl;
  // cout << "\tmovl\t" << "%rax, " << ? << endl;

  _expr->generate();

  if(_expr->_register == nullptr)
    load(_expr, getreg());

  cout << (_type.size() == 1 ? "\tmovsbl\t" : "\tmovl\t") << "(%" << _expr->_register << "), %" << _expr->_register << endl;

  assign(this, _expr->_register);

}

void Address::generate()
{
  _expr->generate();

  if(_expr->_register == nullptr)
    load(_expr, getreg());

  cout << "\tleaq\t" << _expr << ", %rax" << endl;
  // cout << "\tmovq\t" << "%rax, " << ? << endl;

  assign(this, _expr->_register);
}

void LogicalAnd::generate()
{
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  Label label1, label2;
  stringstream ss;
  ss << label1;
  ss << label2;

  // cout << "\tmov\t" << _left << ", %rax" << endl;
  cout << "\tcmp\t" << "$0, %" << _left << endl;
  cout << "je\t" << label2 << endl;

  cout << "\tmov\t" << _right << ", %" << _left << endl;
  cout << "\tcmp\t" << "$0, %" << _left << endl;
  cout << "\tje\t" << label2 << endl;
  cout << "\tjmp\t" << label1 << endl;
  cout << label2 << ":" << endl;
  cout << "\tmov\t$0, " << _left << endl;
  cout << label1 << ":" << endl;
  cout << "\tmov\t$1, " << _left << endl;

  assign(this, _left->_register);

}


void LogicalOr::generate()
{
  _left->generate();
  _right->generate();

  if(_left->_register == nullptr)
    load(_left, getreg());

  Label label1, label2;
  stringstream ss;
  ss << label1;
  ss << label2;

  // cout << "\tmov\t" << _left << ", %rax" << endl;
  cout << "\tcmp\t" << "$0, %" << _left << endl;
  cout << "jne\t" << label2 << endl;

  cout << "\tmov\t" << _right << ", %" << _left << endl;
  cout << "\tcmp\t" << "$0, %" << _left << endl;
  cout << "\tjne\t" << label2 << endl;
  cout << "\tmov\t$0, " << _left << endl;
  cout << "\tjmp\t" << label1 << endl;
  cout << label2 << ":" << endl;
  cout << "\tmov\t$1, " << _left << endl;

  cout << label1 << ":" << endl;

  assign(this, _left->_register);
}

void If::generate()
{
  Label skip, exit;

  _expr->test(skip, false);
  _thenStmt->generate();
  cout << "\tjmp\t" << exit << endl;
  release();

  cout << skip << ":" << endl;
  _elseStmt->generate();
  release();
  cout << exit << ":" << endl;
}


void While::generate()
{
  Label loop, exit;

  cout << loop << ":" << endl;

  _expr->test(exit, false);
  _stmt->generate();
  release();

  cout << "\tjmp\t" << loop << endl;
  cout << exit << ":" << endl;
}

/*
 * Function:	suffix (private)
 *
 * Description:	Return the suffix for an opcode based on the given size.
 */

static string suffix(unsigned size)
{
    return size == 1 ? "b\t" : (size == 4 ? "l\t" : "q\t");
}


/*
 * Function:	align (private)
 *
 * Description:	Return the number of bytes necessary to align the given
 *		offset on the stack.
 */

static int align(int offset)
{
    if (offset % STACK_ALIGNMENT == 0)
	return 0;

    return STACK_ALIGNMENT - (abs(offset) % STACK_ALIGNMENT);
}


/*
 * Function:	operator << (private)
 *
 * Description:	Write an expression to the specified stream.  This function
 *		first checks to see if the expression is in a register, and
 *		if not then uses its operand.
 */
// QUESTION REFERRING TO SLIDES

static ostream &operator <<(ostream &ostr, Expression *expr)
{
    if (expr->_register != nullptr)
	return ostr << expr->_register;
  // unsigned size = expr->type().size();
  // return ostr << expr->register->name(size);

    return ostr << expr->_operand;
}


/*
 * Function:	Number::generate
 *
 * Description:	Generate code for a number.  Since there is really no code
 *		to generate, we simply update our operand.
 */

void Number::generate()
{
    stringstream ss;


    ss << "$" << _value;
    _operand = ss.str();
}


/*
 * Function:	Identifier::generate
 *
 * Description:	Generate code for an identifier.  Since there is really no
 *		code to generate, we simply update our operand.
 */

void Identifier::generate()
{
    stringstream ss;


    if (_symbol->_offset == 0)
	ss << global_prefix << _symbol->name() << global_suffix;
    else
	ss << _symbol->_offset << "(%rbp)";

    _operand = ss.str();
}


/*
 * Function:	Call::generate
 *
 * Description:	Generate code for a function call.  Arguments are first
 *		evaluated in case any them are in fact other function
 *		calls.  The first six arguments are placed in registers and
 *		any remaining arguments are pushed on the stack from right
 *		to left.  Each argument on the stack always requires eight
 *		bytes, so the stack will always be aligned on a multiple of
 *		eight bytes.  To ensure 16-byte alignment, we adjust the
 *		stack pointer if necessary.
 *
 *		NOT FINISHED: Ignores any return value.
 */

void Call::generate()
{
    unsigned size, bytesPushed = 0;


    /* Generate code for all the arguments first. */

    for (unsigned i = 0; i < _args.size(); i ++)
	_args[i]->generate();


    /* Adjust the stack if necessary. */

    if (_args.size() > NUM_ARGS_IN_REGS) {
	bytesPushed = align((_args.size() - NUM_ARGS_IN_REGS) * SIZEOF_ARG);

	if (bytesPushed > 0)
	    cout << "\tsubq\t$" << bytesPushed << ", %rsp" << endl;
    }


    /* Move the arguments into the correct registers or memory locations. */

    for (int i = _args.size() - 1; i >= 0; i --) {
	size = _args[i]->type().size();

	if (i < NUM_ARGS_IN_REGS) {
	    cout << "\tmov" << suffix(size) << _args[i] << ", ";
	    cout << parameters[i]->name(size) << endl;
	} else {
	    bytesPushed += SIZEOF_ARG;

	    if (isRegister(_args[i]))
		cout << "\tpushq\t" << _args[i]->_register->name() << endl;
	    else if (isNumber(_args[i]) || size == SIZEOF_ARG)
		cout << "\tpushq\t" << _args[i] << endl;
	    else {
		cout << "\tmov" << suffix(size) << _args[i] << ", ";
		cout << rax->name(size) << endl;
		cout << "\tpushq\t%rax" << endl;
	    }
	}
    }


    /* Call the function.  Technically, we only need to assign the number
       of floating point arguments to %eax if the function being called
       takes a variable number of arguments.  But, it never hurts. */

    if (_id->type().parameters() == nullptr)
	cout << "\tmovl\t$0, %eax" << endl;

    cout << "\tcall\t" << global_prefix << _id->name() << endl;


    /* Reclaim the space of any arguments pushed on the stack. */

    if (bytesPushed > 0)
	cout << "\taddq\t$" << bytesPushed << ", %rsp" << endl;
}


/*
 * Function:	Assignment::generate
 *
 * Description:	Generate code for an assignment statement.
 *
 *		NOT FINISHED: Only works if the right-hand side is an
 *		integer literal and the left-hand size is an integer
 *		scalar.
 */

void Assignment::generate()
{
    bool indirection;
    _left->generate(indirection);
    _right->generate();

    if(_left->_register == nullptr)
      load(_left , getreg());
    if(_right->_register == nullptr)
      load(_right , getreg());

    // cout << "\tmovl\t" << _right << ", " << _left << endl;

    if(indirection) { //*p = expr

        if(_left->type().size() == 1)
            cout << "\tmovb\t%" << _right->_register->name(1) << ", (%" << _left << ")" << endl;
        else if(_left->type().size() == 4)
        cout << "\tmovl\t%" << _right->_register->name(4) << ", (%" << _left << ")" << endl;
        else
        cout << "\tmov\t%" << _right << ", (%" << _left << ")" <<endl;

    }
    else {
      if(_left->type().size() == 1)
          cout << "\tmovb\t%" << _right->_register->name(1) << ", %" << _left << endl;
      else if(_left->type().size() == 4)
      cout << "\tmovl\t%" << _right->_register->name(4) << ", %" << _left << endl;
      else
      cout << "\tmov\t%" << _right << ", %" << _left <<endl;
    }
}


// {
//     bool indirect;
//     _right->generate();
//     _left->generate(indirect);
//     cout << "\tmovl\t" << _right << " , %eax" << endl;
//     if(indirect) { //*p = expr
//         cout << "\tmovl\t" << _left << " , %ecx" << endl;
//         if(_left->type().size() == 1)
//             cout << "\tmovb\t %al, (%ecx)" <<endl;
//         else
//             cout << "\tmovl\t %eax, (%ecx)" << endl;
//     }
//     else {
//         if(_left->type().size() == 1)
//             cout << "\tmovb\t %al, " << _left << endl;
//         else
//             cout << "\tmovl\t %eax, " << _left << endl;
//     }
// }


/*
 * Function:	Block::generate
 *
 * Description:	Generate code for this block, which simply means we
 *		generate code for each statement within the block.
 */

void Block::generate()
{
    for (unsigned i = 0; i < _stmts.size(); i ++)
	_stmts[i]->generate();
}


/*
 * Function:	Function::generate
 *
 * Description:	Generate code for this function, which entails allocating
 *		space for local variables, then emitting our prologue, the
 *		body of the function, and the epilogue.
 */

void Function::generate()
{
    int offset = 0;
    unsigned numSpilled = _id->type().parameters()->size();
    const Symbols &symbols = _body->declarations()->symbols();


    /* Assign offsets to all symbols within the scope of the function. */

    allocate(offset);


    /* Generate the prologue, body, and epilogue. */

    cout << global_prefix << _id->name() << ":" << endl;
    cout << "\tpushq\t%rbp" << endl;
    cout << "\tmovq\t%rsp, %rbp" << endl;

    if (SIMPLE_PROLOGUE) {
	offset -= align(offset);
	cout << "\tsubq\t$" << -offset << ", %rsp" << endl;
    } else {
	cout << "\tmovl\t$" << _id->name() << ".size, %eax" << endl;
	cout << "\tsubq\t%rax, %rsp" << endl;
    }

    if (numSpilled > NUM_ARGS_IN_REGS)
	numSpilled = NUM_ARGS_IN_REGS;

    for (unsigned i = 0; i < numSpilled; i ++) {
	unsigned size = symbols[i]->type().size();
	cout << "\tmov" << suffix(size) << parameters[i]->name(size);
	cout << ", " << symbols[i]->_offset << "(%rbp)" << endl;
    }

    temp_offset = offset;
    _body->generate();
    offset = temp_offset;

    cout << "\tmovq\t%rbp, %rsp" << endl;
    cout << "\tpopq\t%rbp" << endl;
    cout << "\tret" << endl << endl;


    /* Finish aligning the stack. */

    if (!SIMPLE_PROLOGUE) {
	offset -= align(offset);
	cout << "\t.set\t" << _id->name() << ".size, " << -offset << endl;
    }

    cout << "\t.globl\t" << global_prefix << _id->name() << endl << endl;
}


/*
 * Function:	generateGlobals
 *
 * Description:	Generate code for any global variable declarations.
 */

void generateGlobals(Scope *scope)
{
    const Symbols &symbols = scope->symbols();

    for (unsigned i = 0; i < symbols.size(); i ++)
	if (!symbols[i]->type().isFunction()) {
	    cout << "\t.comm\t" << global_prefix << symbols[i]->name() << ", ";
	    cout << symbols[i]->type().size() << endl;
	}
}
