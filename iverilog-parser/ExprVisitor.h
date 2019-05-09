#ifndef EXPR_VISITOR_H
#define EXPR_VISITOR_H

#include "Visitor.h"

class ExprVisitor : public Visitor
{
public:
        void visit(PExpr*o)          { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }

        void visit(PEIdent*)         = 0;
        void visit(PETernary*)       = 0;
        void visit(PEConcat*)        = 0;
        void visit(PECallFunction*)  = 0;
        void visit(PENumber*)        = 0;
        void visit(PEEvent*)         = 0;
        void visit(PEBinary*)        = 0;
        void visit(PEUnary*)         = 0;
        void visit(PEString*)        = 0;

        void visit(Module*o)          { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PWire*o)           { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PGate*o)           { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PGAssign*o)        { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PGBuiltin*o)       { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PGModule*o)        { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(Statement*o)       { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PForStatement*o)   { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PProcess*o)        { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PEventStatement*o) { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PCondit*o)         { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PAssign*o)         { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PAssignNB*o)       { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PBlock*o)          { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PCase*o)           { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PCallTask*o)       { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(PDelays*o)         { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
        void visit(data_type_t*o)     { cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << endl; exit(1); }
};

#endif
