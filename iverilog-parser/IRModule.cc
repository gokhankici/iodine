#include <iostream>

#include "IRModule.h"
#include "IRExporterHelper.h"

using namespace std;

void IRModule::addPort(const IRPort &irPort)
{
    ports.push_back(irPort);
}

void IRModule::addVariable(const IRVariable &irVariable)
{
    variables.push_back(irVariable);
}

std::ostream &operator<<(std::ostream &out, const IRVariableType &t)
{
    switch (t)
    {
    case IRVariableType::REGISTER:
        return out << "register";
    case IRVariableType::WIRE:
        return out << "wire";
    default:
        cerr << "missing ir variable type: " << t << endl;
        exit(1);
    }
}

std::ostream &operator<<(std::ostream &out, const IRPortType &t)
{
    switch (t)
    {
    case IRPortType::INPUT:
        return out << "input";
    case IRPortType::OUTPUT:
        return out << "output";
    default:
        cerr << "missing ir port type: " << t << endl;
        exit(1);
    }
}

inline std::ostream &operator<<(std::ostream &out, const IRVariable &irVariable)
{
    return out << irVariable.variableType << "(" << irVariable.name << ")";
}

inline std::ostream &operator<<(std::ostream &out, const IRPort &irPort)
{
    return out << irPort.portType << "(" << irPort.variable << ")";
}

inline std::ostream &operator<<(std::ostream &out, const IREvent &e)
{
    switch (e.eventType)
    {
    case IREventType::POSEDGE:
        return out << "posedge(" << *e.event << ")";
    case IREventType::NEGEDGE:
        return out << "negedge(" << *e.event << ")";
    case IREventType::STAR:
        return out << "star";
    default:
        cerr << "missing ir event type" << endl;
        exit(1);
    }
}

inline std::ostream &operator<<(std::ostream &out, const IRAlwaysBlock &ab)
{
    return out << "always(" << *ab.event << ", " << *ab.statement << ")";
}

std::ostream &operator<<(std::ostream &out, const IRModule &irModule)
{
    return out << "module("
               << irModule.moduleName << ", "
               << irModule.ports << ", "
               << irModule.variables << ", "
               << irModule.gateStatements << ", "
               << irModule.alwaysBlocks << ")";
}