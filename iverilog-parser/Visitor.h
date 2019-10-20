#ifndef PROLOG_VISITOR_H
#define PROLOG_VISITOR_H

#include <iostream>

// forward declare classes
class Module;

class PWire;
class PGate;
class PGAssign;
class PGBuiltin;
class PGModule;

class Statement;
class PProcess;
class PEventStatement;
class PCondit;
class PAssign;
class PAssignNB;
class PBlock;
class PCase;
class PCallTask;
class PForStatement;

class PExpr;
class PEIdent;
class PETernary;
class PEConcat;
class PECallFunction;
class PENumber;
class PEEvent;
class PEBinary;
class PEUnary;
class PEString;

class PDelays;
class data_type_t;

#define VISITOR_FRIENDS                 \
    friend class IRExporter;            \
    friend class FoldNames;             \
    friend class FoldNamesExtended;     \
    friend class UninterpretedFunction; \
    friend class PrologExporter;        \
    friend class IRExprVisitor;         \
    friend class IRStmtVisitor;

class Visitor
{
public:
    virtual void visit(Module*)          = 0;

    virtual void visit(PWire*)           = 0;
    virtual void visit(PGate*)           = 0;
    virtual void visit(PGAssign*)        = 0;
    virtual void visit(PGBuiltin*)       = 0;
    virtual void visit(PGModule*)        = 0;

    virtual void visit(Statement*)       = 0;
    virtual void visit(PProcess*)        = 0;
    virtual void visit(PEventStatement*) = 0;
    virtual void visit(PCondit*)         = 0;
    virtual void visit(PAssign*)         = 0;
    virtual void visit(PAssignNB*)       = 0;
    virtual void visit(PBlock*)          = 0;
    virtual void visit(PCase*)           = 0;
    virtual void visit(PCallTask*)       = 0;
    virtual void visit(PForStatement*)   = 0;

    virtual void visit(PExpr*)           = 0;
    virtual void visit(PEIdent*)         = 0;
    virtual void visit(PETernary*)       = 0;
    virtual void visit(PEConcat*)        = 0;
    virtual void visit(PECallFunction*)  = 0;
    virtual void visit(PENumber*)        = 0;
    virtual void visit(PEEvent*)         = 0;
    virtual void visit(PEBinary*)        = 0;
    virtual void visit(PEUnary*)         = 0;
    virtual void visit(PEString*)        = 0;

    virtual void visit(PDelays*)         = 0;
    virtual void visit(data_type_t*)     = 0;
};

#endif // PROLOG_VISITOR_H
