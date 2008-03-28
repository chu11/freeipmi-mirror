/*
  Copyright (C) 2007-2008 FreeIPMI Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.
  
  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA
*/


#ifndef _PEF_CONFIG_EVENT_FILTER_TABLE_H_
#define _PEF_CONFIG_EVENT_FILTER_TABLE_H_

#include "pef-config.h"
#include "pef-config-sections.h"

struct config_section * pef_config_event_filter_table_section_get (pef_config_state_data_t *state_data, int num);

#endif /* _PEF_CONFIG_EVENT_FILTER_TABLE_H_ */
