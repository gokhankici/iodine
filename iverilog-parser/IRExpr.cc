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
    return out << "const(" << constant << ")";
}

inline std::ostream &IRExpr_Variable::print(ostream &out) const
{
    return out << "var(" << variable << ", " << moduleName << ")";
}

inline std::ostream &operator<<(std::ostream &out, const IRUFOp& op)
{
    if (auto uop = get_if<IRUnaryOp>(&op))
    {
        switch (*uop)
        {
        case IRUnaryOp::ABS:      return out << "abs";
        case IRUnaryOp::AND:      return out << "and";
        case IRUnaryOp::NEGATE:   return out << "negate";
        case IRUnaryOp::NEGATIVE: return out << "negative";
        case IRUnaryOp::NOT:      return out << "not";
        case IRUnaryOp::OR:       return out << "or";
        default:                  cerr << "missing unary op" << endl; exit(1);
        }
    }
    else if (auto bop = get_if<IRBinaryOp>(&op))
    {
        switch (*bop)
        {
        case IRBinaryOp::ADD:         return out << "add";
        case IRBinaryOp::AND:         return out << "and";
        case IRBinaryOp::ARITH_RS:    return out << "arith_rs";
        case IRBinaryOp::BITWISE_AND: return out << "bitwise_and";
        case IRBinaryOp::BITWISE_OR:  return out << "bitwisE_or";
        case IRBinaryOp::CASE_EQ:     return out << "case_eq";
        case IRBinaryOp::CASE_NEQ:    return out << "case_neq";
        case IRBinaryOp::DIV:         return out << "div";
        case IRBinaryOp::EXP:         return out << "exp";
        case IRBinaryOp::GE:          return out << "ge";
        case IRBinaryOp::GT:          return out << "gt";
        case IRBinaryOp::LE:          return out << "le";
        case IRBinaryOp::LOGIC_EQ:    return out << "logic_eq";
        case IRBinaryOp::LOGIC_NEQ:   return out << "logic_neq";
        case IRBinaryOp::LT:          return out << "lt";
        case IRBinaryOp::MOD:         return out << "mod";
        case IRBinaryOp::MUL:         return out << "mul";
        case IRBinaryOp::NAND:        return out << "nand";
        case IRBinaryOp::NOR:         return out << "nor";
        case IRBinaryOp::OR:          return out << "or";
        case IRBinaryOp::SHL:         return out << "shl";
        case IRBinaryOp::SHR:         return out << "shr";
        case IRBinaryOp::SUB:         return out << "sub";
        case IRBinaryOp::XNOR:        return out << "xnor";
        case IRBinaryOp::XOR:         return out << "xor";
        default:                      cerr << "missing binary op" << endl; exit(1);
        }
    }
    else if (auto oop = get_if<IROtherOp>(&op))
    {
        switch (*oop)
        {
        case IROtherOp::CONCAT:         return out << "concat";
        case IROtherOp::WRITE_TO_INDEX: return out << "write_to_index";
        default:                        cerr << "missing other op" << endl; exit(1);
        }
    }
    else if (auto cfop = get_if<IRCallFunctionOp>(&op))
    {
        return out << "call_function_" << cfop->function;
    }
    else
    {
        cerr << "unknown op" << endl;
        exit(1);
    }
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
    return out << "str(" << value << ")";
}

std::ostream &IRExpr_Select::print(ostream &os) const
{
    assert(!indices.empty());
    return os << "select(" << *variable << ", " << indices << ")";
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