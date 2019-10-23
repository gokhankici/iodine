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
    IRExporter(Module *m) : module(m), moduleInstantiation(NULL) {}
    IRExporter(Module *m, PGModule *mi) : module(m), moduleInstantiation(mi) {}

    // -------------------------------------------------------------------------
    // IR Exporting Functions
    // -------------------------------------------------------------------------
    const IRModule *extractModule() const;
    const IRExpr *toIRExpr(PExpr *) const;
    const IRStmt *toIRStmt(PGate *) const;
    const IRStmt *toIRStmt(Statement *) const;
    const IREvent *toIREvent(PEEvent *) const;
    void dumpIR(ostream&) const;
    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------
    // Helper Functions
    // -------------------------------------------------------------------------
    const std::string getWireName(PWire *w) const;
    void setModulePorts(IRModule *) const;
    bool isToplevel() const;
    bool isConstantExpr(PExpr *) const;
    const IRExpr *pform_nameToIRExpr(const pform_name_t &that) const;
    const IRExpr *nameComponentToIRExpr(const perm_string &,
                                        const std::list<index_component_t> &) const;
    static bool moduleExists(const std::string &moduleName);
    static void setModule(const std::string &moduleName, const IRModule *irModule);
    // -------------------------------------------------------------------------

    const Module *getModule() const
    {
        return module;
    }

    const PGModule *getModuleInstantiation() const
    {
        return moduleInstantiation;
    }

private:
    const Module *const module;
    const PGModule *const moduleInstantiation;

    static const string prolog_comment;
    static const string sep;
    static const string sep2;
    static const string missing_id;
    static const string id_prefix;
    static const string nopStmt;

    static std::unordered_map<std::string, const IRModule *> irModules;
};

#endif // PROLOG_EXPORTER_H
