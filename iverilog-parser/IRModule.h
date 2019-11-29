#ifndef IR_MODULE_H
#define IR_MODULE_H

#include <ostream>
#include <string>
#include <vector>

#include "IRExpr.h"
#include "IRStmt.h"

enum class IRPortType
{
    INPUT,
    OUTPUT
};

enum class IRVariableType
{
    WIRE,
    REGISTER
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

enum class IREventType
{
    POSEDGE,
    NEGEDGE,
    STAR
};

class IREvent
{
public:
    IREvent(IREventType t, const IRExpr *e) : eventType(t), event(e) {}
    IREventType getEventType() { return eventType; }
    const IRExpr *getEvent() { return event; }

    friend std::ostream &operator<<(std::ostream &, const IREvent &);

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

    friend std::ostream &operator<<(std::ostream &, const IRAlwaysBlock &);

private:
    const IREvent *event;
    const IRStmt *statement;
};

class IRModule
{
public:
    IRModule() {}
    void addPort(const IRPort &);
    void addVariable(const IRVariable &);

    void setModuleName(const char *value) { moduleName = value; }
    void addGateStatement(const IRStmt *stmt) { gateStatements.push_back(stmt); }
    void addAlwaysBlock(const IRAlwaysBlock *ab) { alwaysBlocks.push_back(ab); }
    void addModuleInstance(const IRStmt *stmt) { moduleInstances.push_back(stmt); }

    friend std::ostream &operator<<(std::ostream &, const IRModule &);

private:
    std::string moduleName;
    std::vector<IRPort> ports;
    std::vector<IRVariable> variables;
    std::vector<const IRStmt *> gateStatements;
    std::vector<const IRStmt *> moduleInstances;
    std::vector<const IRAlwaysBlock *> alwaysBlocks;
};

#endif