#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <assert.h>

#include "IRExpr.h"

using namespace std;

const string IRExpr_Constant::toIRString() const
{
    return constant;
}

const string IRExpr_Variable::toIRString() const
{
    return variable;
}

const string IRExpr_UF::toIRString() const
{
    ostringstream os;
    os << "uf(" << function << ", [";

    for (size_t i = 0; i < operands.size(); i++)
    {
        if (i > 0)
            os << ", ";
        os << operands.at(i)->toIRString();
    }

    os << "])";
    return os.str();
}

void IRExpr_UF::addOperand(const IRExpr *operand)
{
    operands.push_back(operand);
}

const string IRExpr_If::toIRString() const
{
    cerr << "toIRString should not be called on the if expression!" << endl;
    exit(1);
}

const string IRExpr_String::toIRString() const
{
    return "\"" + value + "\"";
}

const string IRExpr_Select::toIRString() const
{
    assert(!indices.empty());
    ostringstream os;
    os << "select(" << '"' << variable << '"';
    for(auto i: indices)
        os << ", " << i->toIRString();
    os << ")";
    return os.str();
}