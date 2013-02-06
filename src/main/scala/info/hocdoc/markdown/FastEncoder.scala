/*
 * Copyright (C) 2013 Bernhard Berger
 * 
 * Based on pegdown (C) 2010-2011 Mathias Doenitz and
 * peg-markdown (C) 2008-2010 John MacFarlane.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package info.hocdoc.markdown

import scala.util.Random

/** 
 * Holds simple HTML encoding logic. 
 * Maybe despites it's name now not so fast as the Java FastEncoder.
 */
object FastEncoder {
  
  def encode(string: String): String = string.map(c => encode(c)).mkString
  
  def encode(c: Char): String = c match {
    case '&' => "&amp;"
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '\'' => "&#39;"
    case _   => c.toString
  }
  
  private val random = new Random(0x2626);

  def obfuscate(email: String): String = {
    email.map(c => random.nextInt(5) match {
      case 0 | 1 => "&#" + c.toInt.toString + ";"
      case 2 | 3 => "&#x" + c.toInt.toHexString + ";"
      case _     => encode(c)
    }).mkString
  }
  
}