#include "IRExporter.h"
#include "IRExprVisitor.h"

void IRExprVisitor::visit(PEIdent *id)
{
    if (id->package_)
    {
        cerr << "PrologExporter @ PEIdent:" << endl;
        cerr << "identifier has a package name" << endl;
        exit(1);
    }

    irExpr = irExporter->pform_nameToIRExpr(id->path_);
}

void IRExprVisitor::visit(PETernary *te)
{
    irExpr = new IRExpr_If(toIRExpr(te->expr_),
                           toIRExpr(te->tru_),
                           toIRExpr(te->fal_));
}

void IRExprVisitor::visit(PEConcat *cat)
{
    IRExpr_UF *uf = new IRExpr_UF(IROtherOp::CONCAT);
    for (auto itr = cat->parms_.begin(); itr != cat->parms_.end(); ++itr)
    {
        uf->addOperand(toIRExpr(*itr));
    }
    irExpr = uf;
}

void IRExprVisitor::visit(PECallFunction *cf)
{
    ostringstream oss;
    oss << cf->path_;
    string name = oss.str();

    if (cf->parms_.size() == 1 &&
        (name == "$signed" || name == "$unsigned"))
    {
        cf->parms_[0]->accept(this);
        return;
    }

    IRExpr_UF *uf = new IRExpr_UF(IRCallFunctionOp(name));
    for (auto parmE : cf->parms_)
    {
        uf->addOperand(toIRExpr(parmE));
    }
    irExpr = uf;
}

static void print_number(ostream &, const verinum &);

void IRExprVisitor::visit(PENumber *n)
{
    ostringstream oss;
    print_number(oss, n->value());
    irExpr = new IRExpr_Constant(oss.str());
}

void IRExprVisitor::visit(PEEvent *)
{
    cerr << "IRExprVisitor::PEEvent is not defined" << endl;
    exit(1);
}

void IRExprVisitor::visit(PEBinary *be)
{
    IRBinaryOp fun;
    switch (be->op_)
    {
    case 'a': fun = IRBinaryOp::AND;         break; // &&
    case 'o': fun = IRBinaryOp::OR;          break; // ||
    case 'e': fun = IRBinaryOp::LOGIC_EQ;    break; // ==
    case 'n': fun = IRBinaryOp::LOGIC_NEQ;   break; // !=
    case 'l': fun = IRBinaryOp::SHL;         break; // <<
    case 'r': fun = IRBinaryOp::SHR;         break; // >>
    case '&': fun = IRBinaryOp::BITWISE_AND; break; // &
    case '|': fun = IRBinaryOp::BITWISE_OR;  break; // |
    case '<': fun = IRBinaryOp::LT;          break; // <
    case '>': fun = IRBinaryOp::GT;          break; // >
    case '^': fun = IRBinaryOp::XOR;         break; // ^
    case '+': fun = IRBinaryOp::ADD;         break; // +
    case '-': fun = IRBinaryOp::SUB;         break; // -
    case '*': fun = IRBinaryOp::MUL;         break; // *
    case '/': fun = IRBinaryOp::DIV;         break; // /
    case '%': fun = IRBinaryOp::MOD;         break; // %
    case 'R': fun = IRBinaryOp::ARITH_RS;    break; // >>>
    case 'G': fun = IRBinaryOp::GE;          break; // >=
    case 'p': fun = IRBinaryOp::EXP;         break; // **
    case 'E': fun = IRBinaryOp::CASE_EQ;     break; // ===
    case 'L': fun = IRBinaryOp::LE;          break; // <=
    case 'N': fun = IRBinaryOp::CASE_NEQ;    break; // !==
    default:
        cerr << endl
             << "NOT SUPPORTED: Binary expr operand: " << be->op_ << endl;
        be->dump(cerr);
        exit(1);
        //    default:  out << op_;   break;
    }

    irExpr = new IRExpr_UF(fun, toIRExpr(be->left_), toIRExpr(be->right_));
}

void IRExprVisitor::visit(PEUnary *ue)
{
    IRUnaryOp fun;
    switch (ue->op_)
    {
    case 'm': fun = IRUnaryOp::ABS;      break;
    case '!': fun = IRUnaryOp::NOT;      break;
    case '~': fun = IRUnaryOp::NEGATE;   break;
    case '&': fun = IRUnaryOp::AND;      break;
    case '|': fun = IRUnaryOp::OR;       break;
    case '-': fun = IRUnaryOp::NEGATIVE; break;
    default:
        cerr << endl
             << "NOT SUPPORTED: Unary expr operand: " << ue->op_ << endl;
        exit(1);
    }

    irExpr = new IRExpr_UF(fun, toIRExpr(ue->expr_));
}

void IRExprVisitor::visit(PEString *s)
{
    irExpr = new IRExpr_String(s->text_);
}

static void print_number(ostream &o, const verinum &v)
{
    if (v.is_string())
    {
        cerr << endl
             << "NOT SUPPORTED: Number as a string: " << v.as_string() << endl;
        exit(1);
    }

    /* If the number is fully defined (no x or z) then print it
       out as a decimal number. */
    unsigned dec_len = 8 * sizeof(int); /* avoid 32/64 bit differences. */
    if (!v.has_sign())
        dec_len -= 1; /* an unsigned number. */
    if (v.is_defined() && v.len() <= dec_len)
    {
        if (v.has_sign())
            // o << "'sd" << v.as_long();
            o << v.as_long();
        else
            // o << "'d" << v.as_ulong();
            o << v.as_ulong();
        return;
    }

    o << "0b";

    if (v.len() == 0)
    {
        o << "0";
        return;
    }

    verinum::V trim_left = v.get(v.len() - 1);
    unsigned idx;

    if (v.has_sign())
    {
        for (idx = v.len() - 1; idx > 0; idx -= 1)
            if (trim_left != v.get(idx - 1))
                break;

        o << trim_left;
    }
    else
    {
        idx = v.len();
    }

    while (idx > 0)
    {
        o << v.get(idx - 1);
        idx -= 1;
    }
}