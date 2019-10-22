#include <string>
#include <vector>
#include <sstream>

#include "IRExpr.h"
#include "IRStmt.h"

using namespace std;

void IRStmt_Sequence::addStmt(const IRStmt *stmt)
{
    statements.push_back(stmt);
}

const string IRStmt_Sequence::toIRString() const
{
    ostringstream os;
    os << "block([";

    for (size_t i = 0; i < statements.size(); i++)
    {
        if (i > 0)
            os << ", ";
        os << statements.at(i)->toIRString();
    }

    os << "])";
    return os.str();
}

const string IRStmt_Assignment::toIRString() const
{
    ostringstream os;

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

    os << lhs << ", " << rhs->toIRString() << ")";

    return os.str();
}

const string IRStmt_If::toIRString() const
{
    ostringstream os;

    os << "ite("
       << condition->toIRString() << ", "
       << thenStmt->toIRString() << ", "
       << elseStmt->toIRString() << ")";

    return os.str();
}

const string IRStmt_Skip::toIRString() const
{
    return "skip";
}

const string IRStmt_ModuleInstance::toIRString() const
{
    ostringstream os;

    os << "mod_inst("
       << module_type << ", "
       << module_name << ", "
       << "[";

    bool isFirst = true;
    for (auto p : portMapping)
    {
        if (!isFirst)
            os << ", ";
        os << "("
           << "\"" << p.first << "\""
           << ", " << p.second->toIRString()
           << ")";
        isFirst = false;
    }

    os << "])";
    return os.str();
}