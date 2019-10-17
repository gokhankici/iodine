#include <string>
#include <vector>
#include <sstream>

#include "IRExpr.h"

using namespace std;

string IRExpr_Constant::toIRString()
{
    return constant;
}

string IRExpr_Variable::toIRString()
{
    return variable;
}

string IRExpr_UF::toIRString()
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