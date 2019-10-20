#ifndef IR_STMT_VISITOR_H
#define IR_STMT_VISITOR_H

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

#include "IRExpr.h"
#include "IRStmt.h"
#include "Visitor.h"

class IRStmtVisitor : public Visitor
{
public:
    IRStmtVisitor() {}

    void visit(Module* o)         { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }

    void visit(PWire* o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PGate* o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PGAssign*)         ;
    void visit(PGBuiltin*)        ;
    void visit(PGModule*)         ;

    void visit(Statement*)        ;
    void visit(PProcess*)         ;
    void visit(PEventStatement*)  ;
    void visit(PCondit*)          ;
    void visit(PAssign*)          ;
    void visit(PAssignNB*)        ;
    void visit(PBlock*)           ;
    void visit(PCase*)            ;
    void visit(PCallTask*)        ;
    void visit(PForStatement*)    ;

    void visit(PExpr* o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEIdent* o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PETernary* o)      { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEConcat* o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PECallFunction* o) { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PENumber* o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEEvent* o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEBinary* o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEUnary* o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEString* o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }

    void visit(PDelays* o)        { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(data_type_t* o)    { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }

    const IRStmt *getIRStmt()
    {
        assert(irStmt != NULL);
        return irStmt;
    }

private:
    IRStmt* irStmt = NULL;
};

#endif