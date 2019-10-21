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
    IRExprVisitor condV(irExporter);
    IRExprVisitor thenV(irExporter);
    IRExprVisitor elseV(irExporter);

    te->expr_->accept(&condV);
    te->tru_->accept(&thenV);
    te->fal_->accept(&elseV);

    irExpr = new IRExpr_If(condV.getIRExpr(),
                           thenV.getIRExpr(),
                           elseV.getIRExpr());
}

void IRExprVisitor::visit(PEConcat *cat)
{
    IRExpr_UF *uf = new IRExpr_UF("concat");
    for (auto itr = cat->parms_.begin(); itr != cat->parms_.end(); ++itr)
    {
        IRExprVisitor v(irExporter);
        (*itr)->accept(&v);
        uf->addOperand(v.getIRExpr());
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

    IRExpr_UF *uf = new IRExpr_UF("verilog(" + name + ")");
    for (auto parmE : cf->parms_)
    {
        IRExprVisitor v(irExporter);
        parmE->accept(&v);
        uf->addOperand(v.getIRExpr());
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
    case 'a':
        fun = "and";
        break; // &&
    case 'o':
        fun = "or";
        break; // ||
    case 'e':
        fun = "eq";
        break; // ==
    case 'n':
        fun = "neq";
        break; // !=
    case 'l':
        fun = "shl";
        break; // <<
    case 'r':
        fun = "shr";
        break; // >>
    case '&':
        fun = "bitwise-and";
        break; // &
    case '|':
        fun = "bitwise-or";
        break; // |
    case '<':
        fun = "lt";
        break; // <
    case '>':
        fun = "gt";
        break; // >
    case '^':
        fun = "xor";
        break; // ^
    case '+':
        fun = "add";
        break; // +
    case '-':
        fun = "sub";
        break; // -
    case '*':
        fun = "mul";
        break; // *
    case '/':
        fun = "div";
        break; // /
    default:
        cerr << endl
             << "NOT SUPPORTED: Binary expr operand: " << be->op_ << endl;
        exit(1);
        //    case 'E': out << "==="; break;
        //    case 'L': out << "<=";  break;
        //    case 'N': out << "!=="; break;
        //    case 'p': out << "**";  break;
        //    case 'R': out << ">>>"; break;
        //    default:  out << op_;   break;
    }

    IRExpr_UF *uf = new IRExpr_UF(fun);

    IRExprVisitor lv(irExporter), rv(irExporter);
    be->left_->accept(&lv);
    be->right_->accept(&rv);

    uf->addOperand(lv.getIRExpr());
    uf->addOperand(rv.getIRExpr());

    irExpr = uf;
}

void IRExprVisitor::visit(PEUnary *ue)
{
    const char *fun;
    switch (ue->op_)
    {
    case 'm':
        fun = "abs";
        break;
    case '!':
        fun = "not";
        break;
    case '~':
        fun = "neg";
        break;
    default:
        cerr << endl
             << "NOT SUPPORTED: Unary expr operand: " << ue->op_ << endl;
        exit(1);
    }

    IRExpr_UF *uf = new IRExpr_UF(fun);

    IRExprVisitor v(irExporter);
    ue->expr_->accept(&v);

    uf->addOperand(v.getIRExpr());

    irExpr = uf;
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