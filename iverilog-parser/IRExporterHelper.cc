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
#include "IRExpr.h"
#include "IRStmt.h"

#define UNW_LOCAL_ONLY
#include <cxxabi.h>
#include <libunwind.h>
#include <cstdio>
// #include <cstdlib>

using namespace std;

extern std::map<perm_string, Module *> pform_modules;

const string IRExporter::prolog_comment = "% ";
const string IRExporter::sep = "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%";
const string IRExporter::sep2 = "%-------------------------------------------------------------------------------";
const string IRExporter::missing_id = "id_MISSING_ID";
const string IRExporter::id_prefix = ""; // "v_";
const string IRExporter::nopStmt = "skip";

bool IRExporter::isToplevel()
{
    return moduleInstantiation == NULL;
}

IRExpr *IRExporter::nameComponentToIRExpr(const perm_string &name, const std::list<index_component_t> &indices)
{
    string nameStr(name.str());
    bool varExists = false;
    PExpr *paramExp = NULL;

    // check if variable is a parameter
    if (auto paramItr = module->parameters.find(name); paramItr != module->parameters.end())
    {
        varExists = true;
        paramExp = paramItr->second.expr;
    }
    else if (auto localParamItr = module->localparams.find(name); localParamItr != module->localparams.end())
    {
        varExists = true;
        paramExp = localParamItr->second.expr;
    }

    if (paramExp)
    {
        if (moduleInstantiation == NULL)
        {
            return toIRExpr(paramExp);
        }

        if (moduleInstantiation->overrides_ && (!moduleInstantiation->overrides_->empty()))
        {
            auto orItr = moduleInstantiation->overrides_->begin();
            auto allParamsItr = module->param_names.begin();

            while (allParamsItr != module->param_names.end())
            {
                perm_string paramName = *allParamsItr;
                PExpr *overriddenExp = *orItr;

                if (strcmp(paramName.str(), name.str()) == 0)
                {
                    if (overriddenExp == NULL)
                    {
                        break;
                    }
                    return toIRExpr(overriddenExp);
                }

                ++orItr;
                ++allParamsItr;
            }
        }

        if (moduleInstantiation->parms_)
        {
            assert(moduleInstantiation->overrides_ == NULL);

            for (unsigned i = 0; i < moduleInstantiation->nparms_; i++)
            {
                named<PExpr *> &n = moduleInstantiation->parms_[i];

                if (strcmp(n.name.str(), name.str()) == 0)
                {
                    return toIRExpr(n.parm);
                }
            }
        }

        return toIRExpr(paramExp);
    }

    if (!varExists)
    {
        varExists =
            (module->wires.find(name) != module->wires.end()) ||
            (module->funcs.find(name) != module->funcs.end());
    }

    if (!varExists)
    {
        cerr << endl
             << "cannot find variable '" << nameStr << "' in module " << module->mod_name() << endl;
        exit(1);
    }

    ostringstream os;

    os << id_prefix << nameStr;

    string i = os.str();

    if (!indices.empty())
    {
        IRExpr_UF *indexExpr = new IRExpr_UF("indexExpr");
        for (auto idx = indices.begin(); idx != indices.end(); ++idx)
        {
            const index_component_t &ic = (*idx);
            switch (ic.sel)
            {
            case index_component_t::SEL_BIT:
                indexExpr->addOperand(toIRExpr(ic.msb));
                break;
            case index_component_t::SEL_PART:
            case index_component_t::SEL_IDX_UP:
            case index_component_t::SEL_IDX_DO:
                indexExpr->addOperand(toIRExpr(ic.msb));
                indexExpr->addOperand(toIRExpr(ic.lsb));
                break;
            case index_component_t::SEL_BIT_LAST:
            default:
                break;
            }
        }

        return indexExpr;
    }
    else
    {
        return new IRExpr_Variable(i);
    }
}

IRExpr *IRExporter::pform_nameToIRExpr(const pform_name_t &that)
{
    pform_name_t::const_iterator cur;

    cur = that.begin();
    const name_component_t &n = *cur;
    IRExpr *result = nameComponentToIRExpr(n.name, n.index);

    ++cur;

    if (cur != that.end())
    {
        cerr << endl
             << "NOT SUPPORTED: multiple name components: " << that << endl;
        exit(1);
    }

    return result;
}

const string IRExporter::getWireName(PWire *w)
{
    return nameComponentToIRExpr(w->basename(), std::list<index_component_t>())->toIRString();
}

const IREvent *IRExporter::toIREvent(const PEEvent *)
{
    // TODO
    return NULL;
}