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
    IRExpr_UF *uf = new IRExpr_UF("concat");
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

    IRExpr_UF *uf = new IRExpr_UF("call_function(" + name + ")");
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
    const char *fun;
    switch (be->op_)
    {
    case 'a': fun = "and";         break; // &&
    case 'o': fun = "or";          break; // ||
    case 'e': fun = "logic_eq";    break; // ==
    case 'n': fun = "logic_neq";   break; // !=
    case 'l': fun = "shl";         break; // <<
    case 'r': fun = "shr";         break; // >>
    case '&': fun = "bitwise-and"; break; // &
    case '|': fun = "bitwise-or";  break; // |
    case '<': fun = "lt";          break; // <
    case '>': fun = "gt";          break; // >
    case '^': fun = "xor";         break; // ^
    case '+': fun = "add";         break; // +
    case '-': fun = "sub";         break; // -
    case '*': fun = "mul";         break; // *
    case '/': fun = "div";         break; // /
    case '%': fun = "mod";         break; // %
    case 'R': fun = "arith-rs";    break; // >>>
    case 'G': fun = "ge";          break; // >=
    case 'p': fun = "exp";         break; // **
    case 'E': fun = "case_eq";     break; // ===
    case 'L': fun = "le";          break; // <=
    case 'N': fun = "case_neq";    break; // !==
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
    const char *fun;
    switch (ue->op_)
    {
    case 'm': fun = "abs";  break;
    case '!': fun = "not";  break;
    case '~': fun = "neg";  break;
    case '&': fun = "and";  break;
    case '|': fun = "or";   break;
    case '-': fun = "neg";  break;
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