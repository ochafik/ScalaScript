package scalaxy.js

import scala.collection.JavaConversions._
import scala.reflect.api.Universe
import com.google.javascript.jscomp._
import com.google.javascript.rhino._
import com.google.javascript.rhino.jstype._

object SpecialCases {

  val qualNameRx = """(.*?)\.([^.]+)""".r
  // val jsPrimitiveTypes = Set("Object", "Boolean", "String", 
  val invalidOverrideExceptions = Set(
    "DOMApplicationCache.prototype.addEventListener",
    "DOMApplicationCache.prototype.removeEventListener",
    "DOMApplicationCache.prototype.dispatchEvent",
    // "ClipboardData.prototype.clearData",
    "DataTransfer.prototype.clearData",
    "DataTransfer.prototype.setData",
    "DataTransfer.prototype.getData",
    "Object.prototype.toLocaleString",
    "Object.prototype.toSource",
    "Boolean.prototype.toSource",
    "Date.prototype.toJSON",
    "Element.prototype.querySelector"
  )//.map(_.r)

  val missingOverrideExceptions = Set(
    "String.prototype.valueOf",
    "Date.prototype.valueOf",
    "Array.prototype.toSource"
  )//.map(_.r)

  val missingConstructors = SourceFile.fromCode("missing-constructors.js", """
    /** @constructor */ // var DatabaseCallback = function() {};
    /** @constructor */ // var DedicatedWorkerGlobalScope = function() {};
    /** @constructor */ // var EventListener = function() {};
    /** @constructor */ // var EventTarget = function() {};
    /** @constructor */ // var LinkStyle = function() {};
    /** @constructor */ // var Range = function() {};
    /** @constructor */ // var Screen = function() {};
    /** @constructor */ // var SharedWorkerGlobalScope = function() {};
    /** @constructor */ // var Storage = function() {};
    /** @constructor */ // var ViewCSS = function() {};
    /** @constructor */ // var WindowLocalStorage = function() {};
    /** @constructor */ // var WindowSessionStorage = function() {};
    /** @constructor */ // var WorkerGlobalScope = function() {};
    /** @constructor */ // var WorkerLocation = function() {};
    /** @constructor */ // var XMLHttpRequest = function() {};
	""")
}
