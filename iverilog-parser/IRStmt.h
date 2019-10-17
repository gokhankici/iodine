#ifndef IR_STMT_H
#define IR_STMT_H

#include <vector>
#include <string>

#include "IRExpr.h"

// -----------------------------------------------------------------------------
// IR Statements
// -----------------------------------------------------------------------------

/*
data IRStmt = Sequence [IRStmt]
            | Assignment AsgnType String IRExpr
            | IfStmt IRExpr IRStmt IRStmt
            | Skip
*/

class IRStmt
{
public:
    virtual std::string toIRString() = 0;
};

class IRStmt_Sequence : public IRStmt
{
public:
    IRStmt_Sequence() {}
    void addStmt(IRStmt *stmt);
    std::string toIRString();

private:
    std::vector<IRStmt *> statements;
};

enum IRStmt_AssignmentType
{
    ContAsgn,
    BlockingAsgn,
    NonBlockingAsgn
};

class IRStmt_Assignment : public IRStmt
{
public:
    IRStmt_Assignment(IRStmt_AssignmentType type, std::string &lhs, IRExpr *rhs)
        : type(type), lhs(lhs), rhs(rhs)
    {
    }
    std::string toIRString();

private:
    IRStmt_AssignmentType type;
    std::string lhs;
    IRExpr *rhs;
};

class IRStmt_If : public IRStmt
{
public:
    IRStmt_If(IRExpr *condition, IRStmt *thenStmt, IRStmt *elseStmt)
        : condition(condition), thenStmt(thenStmt), elseStmt(elseStmt)
    {
    }
    std::string toIRString();

private:
    IRExpr *condition;
    IRStmt *thenStmt;
    IRStmt *elseStmt;
};

class IRStmt_Skip : public IRStmt
{
public:
    IRStmt_Skip() {}
    std::string toIRString();
};

#endif