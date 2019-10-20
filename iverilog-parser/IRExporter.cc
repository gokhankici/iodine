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
#include <iostream>
#include <iomanip>
#include <typeinfo>
#include <stdlib.h>
#include <sstream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <cstring>

#include "Visitor.h"
#include "IRExporter.h"
#include "IRExprVisitor.h"
#include "IRStmtVisitor.h"

#define UNW_LOCAL_ONLY
#include <cxxabi.h>
#include <libunwind.h>
#include <cstdio>

using namespace std;

extern std::map<perm_string, Module *> pform_modules;

static IRVariableType getVariableType(PWire *w)
{
    switch (w->get_wire_type())
    {
    case NetNet::WIRE:
    case NetNet::IMPLICIT:
        return IRVariableType::IR_WIRE;
    case NetNet::REG:
    case NetNet::IMPLICIT_REG:
        return IRVariableType::IR_REGISTER;
    default:
        cerr << "Not supported net:" << endl;
        w->dump(cerr, 0);
        exit(1);
    }
}

IRModule *IRExporter::extractModule()
{
    IRModule *irModule = new IRModule;
    setModulePorts(irModule);

    // add registers and wires to the module
    for (map<perm_string, PWire *>::const_iterator wire = module->wires.begin(); wire != module->wires.end(); ++wire)
    {
        PWire *w = (*wire).second;
        IRVariable v(getVariableType(w), getWireName(w));
        irModule->addVariable(v);
    }

    // PGAssign, PGBuiltin, and PGModule
    for (auto gateItr = module->get_gates().begin(); gateItr != module->get_gates().end(); ++gateItr)
    {
        PGate *pg = *gateItr;
        const IRStmt *stmt;
        if (PGAssign *assignStmt = dynamic_cast<PGAssign *>(pg))
        {
            stmt = toIRStmt(assignStmt);
        }
        else if (PGBuiltin *builtinStmt = dynamic_cast<PGBuiltin *>(pg))
        {
            stmt = toIRStmt(builtinStmt);
        }
        else if (PGModule *moduleStmt = dynamic_cast<PGModule *>(pg))
        {
            stmt = toIRStmt(moduleStmt);
        }
        else
        {
            cerr << "Unknown PGate:" << endl;
            pg->dump(cerr, 0);
            exit(1);
        }

        irModule->addGateStatement(stmt);
    }

    // always blocks
    vector<const PEventStatement *> alwaysBlocks;
    for (list<PProcess *>::const_iterator idx = module->behaviors.begin(); idx != module->behaviors.end(); ++idx)
    {
        const PProcess *process = *idx;
        switch (process->type())
        {
        case IVL_PR_INITIAL:
            // skipping initial block
            break;
        case IVL_PR_FINAL:
            cerr << "final blocks are not supported:" << endl;
            process->dump(cerr, 0);
            exit(1);
        case IVL_PR_ALWAYS:
            const PEventStatement *eventStmt = dynamic_cast<PEventStatement *>(process->statement_);
            const IREvent *irEvent;

            if (eventStmt->expr_.count() == 0)
            {
                irEvent = new IREvent(IR_STAR, NULL);
            }
            else if (eventStmt->expr_.count() == 1)
            {
                if (eventStmt->expr_[0] == NULL)
                {
                    cerr << "PEventStatement: event expression is wait_fork" << endl;
                    eventStmt->dump(cerr, 0);
                    exit(1);
                }
                irEvent = toIREvent(eventStmt->expr_[0]);
            }
            else
            {
                cerr << "PEventStatement: multiple event expression" << endl;
                eventStmt->dump(cerr, 0);
                exit(1);
            }

            Statement *statement = eventStmt->statement_;
            IRAlwaysBlock *ab = new IRAlwaysBlock(irEvent, toIRStmt(statement));
            irModule->addAlwaysBlock(ab);
            break;
        }
    }

    // ###########################################################################
    // missing functionality
    // ###########################################################################
    // skipping variable initializations
    if (module->var_inits.size() != 0)
    {
        out << endl
            << prolog_comment << "skipping variable initializations" << endl;
        for (unsigned i = 0; i < module->var_inits.size(); i++)
        {
            Statement *s = module->var_inits[i];
            PAssign *ba = dynamic_cast<PAssign *>(s);
            assert(ba != NULL);

            PExpr *rexpr = ba->rval();
            if (rexpr == NULL)
            {
                // don't know what to do, so continue
                continue;
            }

            if (!isConstantExpr(rexpr))
            {
                cerr << endl;
                s->dump(cerr, 0);
                cerr << endl
                     << "initial assignment is not a constant blocking assignment" << endl;
                exit(1);
            }
        }
    }

    // attributes
    if (module->attributes.begin() != module->attributes.end())
    {
        cerr << "NOT SUPPORTED: attributes" << endl;
        exit(1);
    }

    // typedefs
    if (module->typedefs.begin() != module->typedefs.end())
    {
        cerr << "NOT SUPPORTED: typedefs" << endl;
        exit(1);
    }

    // specparams
    if (module->specparams.begin() != module->specparams.end())
    {
        cerr << "NOT SUPPORTED: specparams" << endl;
        exit(1);
    }

    // enumerations
    if (module->enum_sets.begin() != module->enum_sets.end())
    {
        cerr << "NOT SUPPORTED: enumerations" << endl;
        exit(1);
    }

    // classes
    if (module->classes.begin() != module->classes.end())
    {
        cerr << "NOT SUPPORTED: classes" << endl;
        exit(1);
    }

    // nested modules
    if (module->nested_modules.begin() != module->nested_modules.end())
    {
        cerr << "NOT SUPPORTED: nested modules" << endl;
        exit(1);
    }

    // generate variables
    if (module->genvars.begin() != module->genvars.end())
    {
        cerr << "NOT SUPPORTED: genvars" << endl;
        exit(1);
    }

    // generate schemes
    if (module->generate_schemes.begin() != module->generate_schemes.end())
    {
        cerr << "NOT SUPPORTED: generate schemes" << endl;
        exit(1);
    }

    // defparms
    if (module->defparms.begin() != module->defparms.end())
    {
        cerr << "NOT SUPPORTED: defparms" << endl;
        exit(1);
    }

    // events
    if (module->events.begin() != module->events.end())
    {
        cerr << "NOT SUPPORTED: events" << endl;
        exit(1);
    }

    // tasks
    if (module->tasks.begin() != module->tasks.end())
    {
        cerr << "NOT SUPPORTED: tasks" << endl;
        exit(1);
    }

    // analog behaviors
    if (module->analog_behaviors.begin() != module->analog_behaviors.end())
    {
        cerr << "NOT SUPPORTED: analog behaviors" << endl;
        exit(1);
    }

    // specify paths
    if (module->specify_paths.begin() != module->specify_paths.end())
    {
        cerr << "NOT SUPPORTED: specify paths" << endl;
        exit(1);
    }

    return irModule;
}

void IRExporter::setModulePorts(IRModule *irModule)
{
    irModule->setTopLevel(isToplevel());
    irModule->setModuleName(this->module->mod_name().str());

    if (!isToplevel())
    {
        irModule->setInstanceName(this->moduleInstantiation->get_name().str());
    }

    // print formal parameters
    if (module->port_count() > 0)
    {
        for (unsigned i = 0; i < module->port_count(); i++)
        {
            Module::port_t *p = module->ports[i];
            if (p == NULL)
            {
                cerr << "unconnected port in " << module->mod_name() << endl;
                exit(1);
            }

            string portname = nameComponentToIRExpr(p->name, std::list<index_component_t>())->toIRString();
            string firstExprStr = pform_nameToIRExpr(p->expr[0]->path())->toIRString();

            if (p->expr.size() != 1 || portname.compare(firstExprStr) != 0)
            {
                cerr << "port in " << module->mod_name() << " has weird internal connections" << endl;
                cerr << "    ." << p->name << "(" << *p->expr[0];
                for (unsigned wdx = 1; wdx < p->expr.size(); wdx += 1)
                {
                    cerr << ", " << *p->expr[wdx];
                }
                cerr << ")";
                exit(1);
            }

            auto wireItr = module->wires.find(p->name);
            PWire *wire = NULL;

            if (wireItr == module->wires.end())
            {
                cerr << portname << " not found in module->wires" << endl;
                exit(1);
            }
            wire = wireItr->second;
            assert(wire != NULL);

            IRPortType portType;

            switch (wire->get_port_type())
            {
            case PortType::PINPUT:
                portType = IRPortType::IR_INPUT;
                break;
            case PortType::POUTPUT:
                portType = IRPortType::IR_OUTPUT;
                break;
            default:
            {
                cerr << portname << " is not 'input' or 'output'" << endl;
                wire->dump(cerr, 0);
                exit(1);
            }
            }

            IRPort irPort(portType, getVariableType(wire), portname);
            irModule->addPort(irPort);
        }
    }
}

const IRExpr *IRExporter::toIRExpr(PExpr *expr)
{
    IRExprVisitor v;
    expr->accept(&v);
    return v.getIRExpr();
}

const IRStmt *IRExporter::toIRStmt(PGate *pgate)
{
    IRStmtVisitor v;
    pgate->accept(&v);
    return v.getIRStmt();
}

const IRStmt *IRExporter::toIRStmt(Statement *stmt)
{
    IRStmtVisitor v;
    stmt->accept(&v);
    return v.getIRStmt();
}

bool IRExporter::isConstantExpr(PExpr *expr)
{
    const IRExpr* irExpr = toIRExpr(expr);
    return dynamic_cast<const IRExpr_Constant*>(irExpr) != NULL;
}