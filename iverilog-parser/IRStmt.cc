#include <string>
#include <vector>
#include <sstream>

#include "IRExpr.h"
#include "IRStmt.h"

using namespace std;

void IRStmt_Sequence::addStmt(IRStmt *stmt)
{
    statements.push_back(stmt);
}

string IRStmt_Sequence::toIRString()
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

string IRStmt_Assignment::toIRString()
{
    ostringstream os;

    switch (type)
    {
    case NonBlockingAsgn:
        os << "nb_asgn(";
        break;
    case BlockingAsgn:
        os << "b_asgn(";
        break;
    case ContAsgn:
        os << "b_asgn(";
        break;
    }

    os << lhs << ", " << rhs->toIRString() << ")";

    return os.str();
}

string IRStmt_If::toIRString()
{
    ostringstream os;

    os << "ite("
       << condition->toIRString() << ", "
       << thenStmt->toIRString() << ", "
       << elseStmt->toIRString() << ")";

    return os.str();
}

string IRStmt_Skip::toIRString()
{
    return "skip";
}