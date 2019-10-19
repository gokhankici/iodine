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
#include "ExprVisitor.h"

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
        irModule->addVariable(getVariableType(w), getWireName(w));
    }

    // ###########################################################################
    // PGAssign, PGBuiltin, and PGModule
    // ###########################################################################
    IRStmt_Sequence gateStatements;
    for (auto gateItr = module->get_gates().begin(); gateItr != module->get_gates().end(); ++gateItr)
    {
        // TODO
        IRStmt *stmt = toIRStmt(*gateItr);
        gateStatements.addStmt(stmt);
    }
    out << gateStatements.toIRString() << ",";

    // ###########################################################################
    // always, initial and final blocks
    // ###########################################################################
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
            alwaysBlocks.push_back(dynamic_cast<PEventStatement *>(process->statement_));
            break;
        }
    }
    out << "[";
    for (size_t i = 0; i < alwaysBlocks.size(); i++)
    {
        if (i > 0)
            out << ", ";
        out << alwaysBlocktoIRStmt(alwaysBlocks.at(i))->toIRString();
    }
    out << "])";

    if (isToplevel())
    {
        out << "." << endl
            << endl;
    }

    out << endl
        << sep2 << endl;

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

            irModule->addPort(portType, getVariableType(wire), portname);
        }
    }
}

IRExpr *IRExporter::toIRExpr(const PExpr *)
{
    // TODO
    return NULL;
}

IRStmt *IRExporter::toIRStmt(const PGate *)
{
    // TODO
    return NULL;
}

IRStmt *IRExporter::alwaysBlocktoIRStmt(const PEventStatement *)
{
    // TODO
    return NULL;
}

bool IRExporter::isConstantExpr(PExpr *)
{
    // TODO
    return false;
}