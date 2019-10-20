#ifndef IR_EXPORTER_H
#define IR_EXPORTER_H

#include <iostream>
#include <vector>
#include <sstream>
#include <cassert>
#include <unordered_set>
#include <unordered_map>

#include "Visitor.h"
#include "ExprVisitor.h"

#include "IRExpr.h"
#include "IRStmt.h"
#include "IRModule.h"

class IRExporter
{
public:
    IRExporter(Module *m) : out(std::cout), module(m), moduleInstantiation(NULL) {}
    IRExporter(Module *m, PGModule *mi) : out(std::cout), module(m), moduleInstantiation(mi) {}

    // -------------------------------------------------------------------------
    // IR Exporting Functions
    // -------------------------------------------------------------------------
    IRModule *extractModule();
    IRExpr *toIRExpr(const PExpr *);
    IRStmt *toIRStmt(const PGAssign *);
    IRStmt *toIRStmt(const PGBuiltin *);
    IRStmt *toIRStmt(const PGModule *);
    IRStmt *toIRStmt(const Statement *);
    IRStmt *alwaysBlocktoIRStmt(const PEventStatement *);
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Helper Functions
    // -------------------------------------------------------------------------
    const std::string getWireName(PWire *w);
    void setModulePorts(IRModule *);
    bool isToplevel();
    bool isConstantExpr(PExpr *);
    IRExpr *pform_nameToIRExpr(const pform_name_t &that);
    IRExpr *nameComponentToIRExpr(const perm_string &, const std::list<index_component_t> &);
    const IREvent *toIREvent(const PEEvent *);
    // -------------------------------------------------------------------------

private:
    ostream &out;
    const Module *module;
    const PGModule *moduleInstantiation;

    static const string prolog_comment;
    static const string sep;
    static const string sep2;
    static const string missing_id;
    static const string id_prefix;
    static const string nopStmt;
};

// class IRExporter : public Visitor
// {
// public:
//     IRExporter(ostream &o, Module* m) :
//         out(o), module(m), moduleInstantiation(NULL)
//     { }

//     IRExporter(IRExporter* pe, Module* m, PGModule* mi) :
//         out(pe->out),
//         module(m), moduleInstantiation(mi)
//     { }

//     IRExporter(IRExporter* pe, ostream &o) :
//         out(o),
//         module(pe->module),
//         moduleInstantiation(pe->moduleInstantiation),
//         portMapping(pe->portMapping)
//     { }

//     void visit(Module*)          ;

//     void visit(PWire*)           ;
//     void visit(PGate*)           ;
//     void visit(PGAssign*)        ;
//     void visit(PGBuiltin*)       ;
//     void visit(PGModule*)        ;

//     void visit(Statement*)       ;
//     void visit(PProcess*)        ;
//     void visit(PEventStatement*) ;
//     void visit(PCondit*)         ;
//     void visit(PAssign*)         ;
//     void visit(PAssignNB*)       ;
//     void visit(PBlock*)          ;
//     void visit(PCase*)           ;
//     void visit(PCallTask*)       ;
//     void visit(PForStatement*)   ;

//     void visit(PExpr*)           ;
//     void visit(PEIdent*)         ;
//     void visit(PETernary*)       ;
//     void visit(PEConcat*)        ;
//     void visit(PECallFunction*)  ;
//     void visit(PENumber*)        ;
//     void visit(PEEvent*)         ;
//     void visit(PEBinary*)        ;
//     void visit(PEUnary*)         ;
//     void visit(PEString*)        ;

//     void visit(data_type_t*)     ;
//     void visit(PDelays*)         ;

// private:
//     unsigned ind = 0;
//     ostream& out;
//     Module* module;
//     PGModule* moduleInstantiation;
//     unordered_map<string,PExpr*> portMapping;

//     const string prolog_comment = "% ";
//     const string sep = "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%";
//     const string sep2 = "%-------------------------------------------------------------------------------";
//     const string missing_id = "id_MISSING_ID";
//     const string id_prefix = ""; // "v_";
//     const string nopStmt = "skip";

//     static unsigned long idCounter;

//     enum AssignType
//     {
//         ContAsgn,
//         BlockingAsgn,
//         NonBlockingAsgn
//     };

//     typedef struct {
//         IRExporter* pe;
//         PExpr* e;
//     } AsgnArg;

//     typedef struct CaseStruct {
//         string     itemVar;
//         Statement* stmt;
//     } CaseStruct;

//     bool isToplevel() const {
//         return moduleInstantiation != NULL;
//     }

//     string export_pform_name_to_prolog(const pform_name_t&that);
//     string export_name_component_to_prolog(const perm_string&, const std::list<index_component_t>&);
//     void export_pins_to_prolog(PGate* g);

//     string exportExpr(PExpr*);

//     void printModuleName(ostream&);
//     void printModuleHeader(ostream&);

//     void printAsgn(IRExporter::AssignType a, PExpr* lhs, PExpr* rhs);

//     void doAsgn(AssignType, AsgnArg, AsgnArg);

//     void printModInstPortAssignments();

//     void doAsgn(AssignType a, PExpr* lhs, PExpr* rhs) {
//         return doAsgn(a, {this, lhs}, {this, rhs});
//     }
// };

#endif // PROLOG_EXPORTER_H
