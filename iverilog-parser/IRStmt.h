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
    virtual std::ostream& print(std::ostream&) const = 0;
};

class IRStmt_Sequence : public IRStmt
{
public:
    IRStmt_Sequence() {}
    void addStmt(const IRStmt *stmt);
    std::ostream& print(std::ostream&) const;

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
    IRStmt_Assignment(IRStmt_AssignmentType t, const IRExpr_Variable *l, const IRExpr *r)
        : type(t), lhs(l), rhs(r)
    {
    }
    std::ostream& print(std::ostream&) const;

private:
    IRStmt_AssignmentType type;
    const IRExpr_Variable* const lhs;
    const IRExpr *const rhs;
};

class IRStmt_If : public IRStmt
{
public:
    IRStmt_If(const IRExpr *c, const IRStmt *t, const IRStmt *e)
        : condition(c), thenStmt(t), elseStmt(e)
    {
    }
    std::ostream& print(std::ostream&) const;

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
    std::ostream& print(std::ostream&) const;

    void setPort(const IRExpr_Variable &, const IRExpr *);
    const IRExpr* getPort(const IRExpr_Variable&) const;

private:
    const std::string module_type;
    const std::string module_name;
    std::unordered_map<IRExpr_Variable,
                       const IRExpr *,
                       IRExpr_Variable::Hash>
        portMapping;
};

class IRStmt_Skip : public IRStmt
{
public:
    IRStmt_Skip() {}
    std::ostream& print(std::ostream&) const;
};

std::ostream &operator<<(std::ostream &, const IRStmt &);

#endif