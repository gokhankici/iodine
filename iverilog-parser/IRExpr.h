#ifndef IR_EXPR_H
#define IR_EXPR_H

#include <vector>
#include <string>

// -----------------------------------------------------------------------------
// IR Expressions
// -----------------------------------------------------------------------------

/*
data IRExpr = Constant String
            | Variable String
            | UninterpretedFunction String [IRExpr]
*/

class IRExpr
{
public:
    virtual std::string toIRString() = 0;
};

class IRExpr_Constant : public IRExpr
{
public:
    IRExpr_Constant(std::string &constant) : constant(constant) {}
    std::string toIRString();

private:
    std::string constant;
};

class IRExpr_Variable : public IRExpr
{
public:
    IRExpr_Variable(std::string &variable) : variable(variable) {}
    std::string toIRString();

private:
    std::string variable;
};

class IRExpr_UF : public IRExpr
{
public:
    IRExpr_UF(std::string &function) : function(function) {}
    IRExpr_UF(std::string &function, IRExpr *op1) : function(function)
    {
        operands.push_back(op1);
    }
    IRExpr_UF(std::string &function, IRExpr *op1, IRExpr *op2) : function(function)
    {
        operands.push_back(op1);
        operands.push_back(op2);
    }
    void addOperand(IRExpr *operand);
    std::string toIRString();

private:
    std::string function;
    std::vector<IRExpr *> operands;
};

#endif