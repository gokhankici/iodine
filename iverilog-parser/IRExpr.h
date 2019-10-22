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
    IRExpr_Constant(const std::string &c) : constant(c) {}
    const std::string toIRString() const;

private:
    const std::string constant;
};

class IRExpr_Variable : public IRExpr
{
public:
    IRExpr_Variable(const std::string &v) : variable(v) {}
    const std::string toIRString() const;

    const std::string &getVariable()
    {
        return variable;
    }

private:
    const std::string variable;
};

class IRExpr_UF : public IRExpr
{
public:
    IRExpr_UF(const char *f) : function(f) {}
    IRExpr_UF(const std::string &f) : function(f) {}
    IRExpr_UF(const char *f, const IRExpr *o) : function(f)
    {
        operands.push_back(o);
    }
    IRExpr_UF(const char *f, const IRExpr *o1, const IRExpr *o2) : function(f)
    {
        operands.push_back(o1);
        operands.push_back(o2);
    }
    void addOperand(const IRExpr *operand);
    const std::string toIRString() const;

private:
    const std::string function;
    std::vector<const IRExpr *> operands;
};

class IRExpr_If : public IRExpr
{
public:
    IRExpr_If(const IRExpr *c, const IRExpr *t, const IRExpr *e)
        : condition(c), thenStmt(t), elseStmt(e)
    {
    }
    const std::string toIRString() const;

private:
    const IRExpr *const condition;
    const IRExpr *const thenStmt;
    const IRExpr *const elseStmt;
};

class IRExpr_String : public IRExpr
{
public:
    IRExpr_String(const std::string &v) : value(v) {}
    const std::string toIRString() const;

private:
    const std::string value;
};

#endif