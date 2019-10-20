#ifndef EXPR_VISITOR_H
#define EXPR_VISITOR_H

#include <iostream>

#include "config.h"
#include "pform.h"
#include "PClass.h"
#include "PEvent.h"
#include "PGenerate.h"
#include "PPackage.h"
#include "PSpec.h"
#include "PTask.h"
#include "discipline.h"
#include "ivl_target_priv.h"

#include "Visitor.h"

class ExprVisitor : public Visitor
{
public:
        void visit(PExpr*o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }

        void visit(PEIdent*)         = 0;
        void visit(PETernary*)       = 0;
        void visit(PEConcat*)        = 0;
        void visit(PECallFunction*)  = 0;
        void visit(PENumber*)        = 0;
        void visit(PEEvent*)         = 0;
        void visit(PEBinary*)        = 0;
        void visit(PEUnary*)         = 0;
        void visit(PEString*)        = 0;

        void visit(Module*o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PWire*o)           { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PGate*o)           { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PGAssign*o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PGBuiltin*o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PGModule*o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(Statement*o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PForStatement*o)   { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PProcess*o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PEventStatement*o) { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PCondit*o)         { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PAssign*o)         { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PAssignNB*o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PBlock*o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PCase*o)           { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PCallTask*o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(PDelays*o)         { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
        void visit(data_type_t*o)     { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
};

#endif
