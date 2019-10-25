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
#include "IRExprVisitor.h"
#include "IRExporterHelper.h"

class IRExporter;

class IRStmtVisitor : public Visitor
{
public:
    IRStmtVisitor(const IRExporter *ire) : irExporter(ire) {}

    void visit(Module* o)         { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }

    void visit(PWire* o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PGate* o)          { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PGAssign*)         ;
    void visit(PGBuiltin*)        ;
    void visit(PGModule*)         ;

    void visit(Statement* o)      { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PProcess* o)       { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PEventStatement* o){ std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); }
    void visit(PCondit*)          ;
    void visit(PAssign*)          ;
    void visit(PAssignNB*)        ;
    void visit(PBlock*)           ;
    void visit(PCase*)            ;
    void visit(PCallTask*)        ;
    void visit(PForStatement* o)  { std::cerr << "NOT SUPPORTED: " << typeid(*this).name() << " @ " << typeid(*o).name() << std::endl; exit(1); } 

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
        if (irStmt == NULL) {
            backtrace();
            assert(irStmt);
        }
        return irStmt;
    }

    const IRExpr *toIRExpr(PExpr *expr) const
    {
        IRExprVisitor v(irExporter);
        expr->accept(&v);
        return v.getIRExpr();
    }

    const IRStmt *toIRStmt(Statement *stmt) const
    {
        IRStmtVisitor v(irExporter);
        stmt->accept(&v);
        return v.getIRStmt();
    }

private:
    const IRStmt *irStmt = NULL;
    const IRExporter *const irExporter;

    const IRStmt *doAssignment(IRStmt_AssignmentType t, PExpr *lhs, PExpr *rhs) const
    {
        return doAssignment(t, toIRExpr(lhs), toIRExpr(rhs));
    }

    const IRStmt *doAssignment(IRStmt_AssignmentType t, PExpr *lhs, const IRExpr *rhs) const
    {
        return doAssignment(t, toIRExpr(lhs), rhs);
    }

    const IRStmt *doAssignment(IRStmt_AssignmentType t, const IRExpr *lhs, const IRExpr *rhs) const;
};

#endif