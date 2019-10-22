#include "IRModule.h"

void IRModule::addPort(const IRPort &irPort)
{
    ports.push_back(irPort);
}

void IRModule::addVariable(const IRVariable &irVariable)
{
    variables.push_back(irVariable);
}

void IRModule::dump(std::ostream &) const
{
    // TODO
}