#ifndef IR_MODULE_H
#define IR_MODULE_H

#include <string>
#include <vector>

#include "IRExpr.h"
#include "IRStmt.h"

enum IRPortType
{
    IR_INPUT,
    IR_OUTPUT
};

enum IRVariableType
{
    IR_WIRE,
    IR_REGISTER
};

class IRVariable
{
public:
    IRVariable(IRVariableType vt, const std::string &n) : variableType(vt), name(n) {}
    const IRVariableType variableType;
    const std::string name;
};

class IRPort
{
public:
    IRPort(IRPortType pt, IRVariableType vt, const std::string &n) : portType(pt), variable(vt, n) {}
    const IRPortType portType;
    const IRVariable variable;
};

enum IREventType
{
    IR_POSEDGE,
    IR_NEGEDGE,
    IR_STAR
};

class IREvent
{
public:
    IREvent(IREventType t, const IRExpr *e) : eventType(t), event(e) {}
    IREventType getEventType() { return eventType; }
    const IRExpr *getEvent() { return event; }

private:
    IREventType eventType;
    const IRExpr *event; // event is NULL if eventType is IR_Star
};

class IRAlwaysBlock
{
public:
    IRAlwaysBlock(const IREvent *e, const IRStmt *s) : event(e), statement(s) {}
    const IREvent *getEvent() { return event; }
    const IRStmt *getStatement() { return statement; }

private:
    const IREvent *event;
    const IRStmt *statement;
};

class IRModule
{
public:
    IRModule() : isTopLevel(false) {}
    void addPort(const IRPort &);
    void addVariable(const IRVariable &);
    void dump(std::ostream &) const;

    void setTopLevel(bool value) { isTopLevel = value; }
    void setModuleName(const char *value) { moduleName = value; }
    void setInstanceName(const char *value) { instanceName = value; }
    void addGateStatement(const IRStmt *stmt) { gateStatements.push_back(stmt); }
    void addAlwaysBlock(const IRAlwaysBlock *ab) { alwaysBlocks.push_back(ab); }

private:
    bool isTopLevel;
    std::string moduleName;
    std::string instanceName;
    std::vector<IRPort> ports;
    std::vector<IRVariable> variables;
    std::vector<const IRStmt *> gateStatements;
    std::vector<const IRAlwaysBlock *> alwaysBlocks;
};

#endif