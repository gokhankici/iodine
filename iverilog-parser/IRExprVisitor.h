#ifndef IR_EXPR_VISITOR_H
#define IR_EXPR_VISITOR_H

#include "IRExpr.h"
#include "ExprVisitor.h"
#include "IRExporterHelper.h"

class IRExporter;

class IRExprVisitor : public ExprVisitor
{
public:
    IRExprVisitor(const IRExporter *ire) : irExporter(ire) {}

    void visit(PEIdent *);
    void visit(PETernary *);
    void visit(PEConcat *);
    void visit(PECallFunction *);
    void visit(PENumber *);
    void visit(PEEvent *);
    void visit(PEBinary *);
    void visit(PEUnary *);
    void visit(PEString *);

    const IRExpr *getIRExpr()
    {
        if (irExpr == NULL)
        {
            backtrace();
            assert(irExpr != NULL);
        }
        return irExpr;
    }

    const IRExpr *toIRExpr(PExpr *expr)
    {
        IRExprVisitor v(irExporter);
        expr->accept(&v);
        return v.getIRExpr();
    }

private:
    const IRExpr *irExpr = NULL;
    const IRExporter *const irExporter;
};

#endif