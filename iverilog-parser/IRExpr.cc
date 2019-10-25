#include <string>
#include <vector>
#include <sstream>
#include <iostream>
#include <assert.h>

#include "IRExpr.h"
#include "IRExporterHelper.h"

using namespace std;

IRExpr::~IRExpr() {}

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

std::ostream &IRExpr_If::print(ostream &out) const
{
    return out << "ite_expr(" << *condition << ", " << *thenExpr << ", " << *elseExpr << ")";
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

bool operator==(const IRExpr_Variable& v1, const IRExpr_Variable& v2)
{
    return v1.variable == v2.variable && v1.moduleName == v2.moduleName;
}