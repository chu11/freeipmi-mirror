/*****************************************************************************\
 *  $Id: ipmi-fru-language-codes-spec.c,v 1.10 2010-02-08 22:09:40 chu11 Exp $
 *****************************************************************************
 *  Copyright (C) 2007-2014 Lawrence Livermore National Security, LLC.
 *  Copyright (C) 2007 The Regents of the University of California.
 *  Produced at Lawrence Livermore National Laboratory (cf, DISCLAIMER).
 *  Written by Albert Chu <chu11@llnl.gov>
 *  UCRL-CODE-232183
 *
 *  This file is part of Ipmi-fru, a tool used for retrieving
 *  motherboard field replaceable unit (FRU) information. For details,
 *  see http://www.llnl.gov/linux/.
 *
 *  Ipmi-fru is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3 of the License, or (at your
 *  option) any later version.
 *
 *  Ipmi-fru is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with Ipmi-fru.  If not, see <http://www.gnu.org/licenses/>.
\*****************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif /* HAVE_CONFIG_H */

#include <stdio.h>
#include <stdlib.h>

#include "freeipmi/spec/ipmi-fru-language-codes-spec.h"

#include "freeipmi-portability.h"

const char *const ipmi_fru_language_codes[] =
  {
    "English",
    "Afar",
    "Abkhazian",
    "Afrikaans",
    "Amharic",
    "Arabic",
    "Assamese",
    "Aymara",
    "Azerbaijani",
    "Bashkir",
    "Byelorussian",
    "Bulgarian",
    "Bihari",
    "Bislama",
    "Bengali; Bangla",
    "Tibetan",
    "Breton",
    "Catalan",
    "Corsican",
    "Czech",
    "Welsh",
    "Danish",
    "German",
    "Bhutani",
    "Greek",
    "English",
    "Esperanto",
    "Spanish",
    "Estonian",
    "Basque",
    "Persian",
    "Finnish",
    "Fiji",
    "Faeroese",
    "French",
    "Frisian",
    "Irish",
    "Scots Gaelic",
    "Galician",
    "Guarani",
    "Gujarati",
    "Hausa",
    "Hindi",
    "Croatian",
    "Hungarian",
    "Armenian",
    "Interlingua",
    "Interlingue",
    "Inupiak",
    "Indonesian",
    "Icelandic",
    "Italian",
    "Hebrew",
    "Japanese",
    "Yiddish",
    "Javanese",
    "Georgian",
    "Kazakh",
    "Greenlandic",
    "Cambodian",
    "Kannada",
    "Korean",
    "Kashmiri",
    "Kurdish",
    "Kirghiz",
    "Latin",
    "Lingala",
    "Laothian",
    "Lithuanian",
    "Latvian, Lettish",
    "Malagasy",
    "Maori",
    "Macedonian",
    "Malayalam",
    "Mongolian",
    "Moldavian",
    "Marathi",
    "Malay",
    "Maltese",
    "burmese",
    "Nauru",
    "Nepali",
    "Dutch",
    "Norwegian",
    "Occitan",
    "(Afan) Oromo",
    "Oriya",
    "Punjabi",
    "Polish",
    "Pashto, Pushto",
    "Portuguese",
    "Quechua",
    "Rhaeto-Romance",
    "Kirundi",
    "Romanian",
    "Russian",
    "Kinyarwanda",
    "Sanskrit",
    "Sindhi",
    "Sangro",
    "Serbo-Croation",
    "Singhalese",
    "Slovak",
    "Slovenian",
    "Samoan",
    "Shona",
    "Somali",
    "Albanian",
    "Serbian",
    "Siswati",
    "Sesotho",
    "Sudanese",
    "Swedish",
    "Swahili",
    "Tamil",
    /* Spec has "Tegulu", likely a typo */
    "Telugu",
    "Tajik",
    "Thai",
    "Tigrinya",
    "Turkmen",
    "Tagalog",
    "Setswana",
    "Tonga",
    "Turkish",
    "Tsonga",
    "Tatar",
    "Twi",
    "Ukranian",
    "Urdu",
    "Uzbek",
    "Vietnamese",
    "Volapuk",
    "Wolof",
    "Xhosa",
    "Yoruba",
    "Chinese",
    "Zulu",
    NULL
  };
