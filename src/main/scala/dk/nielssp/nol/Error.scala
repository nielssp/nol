package dk.nielssp.nol

import java.io.File

import scala.util.parsing.input.{NoPosition, Position}

abstract class Error(msg: String, var pos: Position, var file: File = null) extends Exception(msg)

class SyntaxError(msg: String, pos: Position, file: File = null) extends Error(msg, pos, file)

class TypeError(msg: String, pos: Position, file: File = null) extends Error(msg, pos, file)

class NameError(msg: String, pos: Position, file: File = null) extends Error(msg, pos, file)

class ImportError(msg: String, pos: Position, file: File = null) extends Error(msg, pos, file)

class DomainError(msg: String, pos: Position = NoPosition, file: File = null) extends Error(msg, pos, file)
