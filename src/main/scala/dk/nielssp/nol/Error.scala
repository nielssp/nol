package dk.nielssp.nol

import java.io.File

import scala.util.parsing.input.Position

abstract class Error(msg: String, pos: Position, file: File = null) extends Exception(msg)

class SyntaxError(msg: String, pos: Position, file: File = null) extends Error(msg, pos, file)
