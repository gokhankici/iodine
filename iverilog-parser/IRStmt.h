#ifndef IR_STMT_H
#define IR_STMT_H

#include <vector>
#include <string>
#include <unordered_map>
#include <assert.h>

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
    virtual const std::string toIRString() const = 0;
};

class IRStmt_Sequence : public IRStmt
{
public:
    IRStmt_Sequence() {}
    void addStmt(const IRStmt *stmt);
    const std::string toIRString() const;

private:
    std::vector<const IRStmt *> statements;
};

enum IRStmt_AssignmentType
{
    IR_CONTINUOUS_ASSIGNMENT,
    IR_BLOCKING_ASSIGNMENT,
    IR_NON_BLOCKING_ASSIGNMENT
};

class IRStmt_Assignment : public IRStmt
{
public:
    IRStmt_Assignment(IRStmt_AssignmentType t, std::string &l, IRExpr *r)
        : type(t), lhs(l), rhs(r)
    {
    }
    const std::string toIRString() const;

private:
    IRStmt_AssignmentType type;
    const std::string lhs;
    const IRExpr *const rhs;
};

class IRStmt_If : public IRStmt
{
public:
    IRStmt_If(const IRExpr *c, const IRStmt *t, const IRStmt *e)
        : condition(c), thenStmt(t), elseStmt(e)
    {
    }
    const std::string toIRString() const;

private:
    const IRExpr *const condition;
    const IRStmt *const thenStmt;
    const IRStmt *const elseStmt;
};

class IRStmt_ModuleInstance : public IRStmt
{
public:
    IRStmt_ModuleInstance(const std::string &mt, const std::string &mn)
        : module_type(mt), module_name(mn) {}
    const std::string toIRString() const;

    void setPort(const std::string &portName, const IRExpr *portValue)
    {
        bool ok = portMapping.insert({portName, portValue}).second;
        assert(ok);
    }

private:
    const std::string module_type;
    const std::string module_name;
    std::unordered_map<std::string, const IRExpr *> portMapping;
};

class IRStmt_Skip : public IRStmt
{
public:
    IRStmt_Skip() {}
    const std::string toIRString() const;
};

#endif