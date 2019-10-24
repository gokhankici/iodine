#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <assert.h>

#include "IRExpr.h"
#include "IRExporterHelper.h"

using namespace std;

inline ostream &IRExpr_Constant::print(ostream &out) const
{
    return out << constant;
}

inline std::ostream &IRExpr_Variable::print(ostream &out) const
{
    return out << variable;
}

std::ostream &IRExpr_UF::print(ostream &out) const
{
    return out << "uf(" << function << ", " << operands << ")";
}

void IRExpr_UF::addOperand(const IRExpr *operand)
{
    operands.push_back(operand);
}

std::ostream &IRExpr_If::print(ostream &) const
{
    cerr << "toIRString should not be called on the if expression!" << endl;
    exit(1);
}

inline std::ostream &IRExpr_String::print(ostream &out) const
{
    return out << '"' << value << '"';
}

std::ostream &IRExpr_Select::print(ostream &os) const
{
    assert(!indices.empty());
    return os << "select(" << '"' << variable << '"' << ", " << indices << ")";
}

inline std::ostream &operator<<(std::ostream &out, const IRExpr &expr)
{
    return expr.print(out);
}

std::string IRExpr::toIRString() const
{
    ostringstream os;
    print(os);
    return os.str();
}