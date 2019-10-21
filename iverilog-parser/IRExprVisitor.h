#ifndef IR_EXPR_VISITOR_H
#define IR_EXPR_VISITOR_H

#include "IRExpr.h"
#include "ExprVisitor.h"

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
        assert(irExpr != NULL);
        return irExpr;
    }

private:
    const IRExpr *irExpr = NULL;
    const IRExporter *irExporter;
};

#endif