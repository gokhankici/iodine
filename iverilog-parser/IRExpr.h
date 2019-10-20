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
    virtual const std::string toIRString() const = 0;
};

class IRExpr_Constant : public IRExpr
{
public:
    IRExpr_Constant(std::string &c) : constant(c) {}
    const std::string toIRString() const;

private:
    std::string constant;
};

class IRExpr_Variable : public IRExpr
{
public:
    IRExpr_Variable(std::string &v) : variable(v) {}
    const std::string toIRString() const;

    const std::string &getVariable()
    {
        return variable;
    }

private:
    std::string variable;
};

class IRExpr_UF : public IRExpr
{
public:
    IRExpr_UF(const char *f) : function(f) {}
    IRExpr_UF(const char *f, IRExpr *o) : function(f)
    {
        operands.push_back(o);
    }
    IRExpr_UF(std::string &f, IRExpr *o1, IRExpr *o2) : function(f)
    {
        operands.push_back(o1);
        operands.push_back(o2);
    }
    void addOperand(const IRExpr *operand);
    const std::string toIRString() const;

private:
    std::string function;
    std::vector<const IRExpr *> operands;
};

#endif