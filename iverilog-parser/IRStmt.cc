#include <string>
#include <vector>
#include <sstream>
#include <unordered_map>

#include "IRExpr.h"
#include "IRStmt.h"

#include "IRExporterHelper.h"

using namespace std;

IRStmt::~IRStmt() {}

void IRStmt_Sequence::addStmt(const IRStmt *stmt)
{
    statements.push_back(stmt);
}

void IRStmt_ModuleInstance::setPort(const IRExpr_Variable &port, const IRExpr *value)
{
    bool ok = portMapping.insert({port, value}).second;
    assert(ok);
}

const IRExpr *IRStmt_ModuleInstance::getPort(const IRExpr_Variable &port) const
{
    auto lookup = portMapping.find(port);
    assert(lookup != portMapping.end());
    return lookup->second;
}

std::ostream &IRStmt_Sequence::print(std::ostream &os) const
{
    return os << "block(" << statements << ")";
}

std::ostream &IRStmt_Assignment::print(std::ostream &os) const
{
    switch (type)
    {
    case IR_NON_BLOCKING_ASSIGNMENT:
        os << "nb_asn(";
        break;
    case IR_BLOCKING_ASSIGNMENT:
        os << "b_asn(";
        break;
    case IR_CONTINUOUS_ASSIGNMENT:
        os << "asn(";
        break;
    }

    return os << *lhs << ", " << *rhs << ")";
}

std::ostream &IRStmt_If::print(std::ostream &os) const
{
    return os << "ite_stmt(" << *condition << ", " << *thenStmt << ", " << *elseStmt << ")";
}

inline std::ostream &IRStmt_Skip::print(std::ostream &os) const
{
    return os << "skip";
}

std::ostream &IRStmt_ModuleInstance::print(std::ostream &os) const
{
    os << "mod_inst("
       << module_type << ", "
       << module_name << ", "
       << "[";

    bool isFirst = true;
    for (auto p : portMapping)
    {
        if (!isFirst)
            os << ", ";
        os << "(" << p.first.getOnlyVariableName()
           << ", " << *p.second
           << ")";
        isFirst = false;
    }

    return os << "])";
}

inline std::ostream &operator<<(std::ostream &out, const IRStmt &stmt)
{
    return stmt.print(out);
}
