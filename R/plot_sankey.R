
# Sankey plotting functionality with networkD3

# node values are 0 indexed
giottoSankeyPlan = setClass(
  'giottoSankeyPlan',
  slots = list(
    node_address = 'data.table', # spat_unit, feat_type, col
    node_subset = 'list', # subset vector for node
    node_label = 'character',
    data_type = 'character', # type of info (cell or feat)
    relations = 'data.table' # from, to
  ),
  prototype = list(
    node_address = data.table::data.table(),
    relations = data.table::data.table()
  )
)

setMethod('show', signature = 'giottoSankeyPlan', function(object) {
  cat('giottoSankeyPlan --',
      paste('N:', nrow(object@node_address)), # nodes
      paste('R:', nrow(object@relations))) # relations/links
  cat('\n')
  if (nrow(object@node_address) > 0L) {
    print(sankeySetAddresses(object))
  }
  if (nrow(object@relations) > 0L) {
    rels = head(object@relations)
    if (nrow(object@relations) > 6L) cat('first 6 relations...')
    print(rels)
  }
  cat('\n')
})

# generics
setGeneric('sankeyRelate', function(x, ...) standardGeneric('sankeyRelate'))
setGeneric('sankeyRelate<-', function(x, add, value) standardGeneric('sankeyRelate<-'))

# methods



# get relations ####
setMethod('sankeyRelate', signature('giottoSankeyPlan'), function(x, ...) {
  return(x@relations)
})

# replace relations ####
setMethod(
  'sankeyRelate<-',
  signature(x = 'giottoSankeyPlan', add = 'logical', value = 'data.frame'),
  function(x, add, value)
  {
    if (nrow(x@relations) == 0L || # no entries
        !add) {
      if(ncol(value) != 2) stop('2 columns expected')
      if(!all(colnames(value) == c('from', 'to')))
        stop("Cols 'from' and 'to' expected")
      dt = data.table::as.data.table(value)
      x@relations = value # direct replacement
    } else {
      x@relations = rbind(x@relations, value) %>%
        unique()
    }

    return(x)
  }
)

setMethod(
  'sankeyRelate<-',
  signature(x = 'giottoSankeyPlan', add = 'logical', value = 'numeric'),
  function(x, add, value)
  {
    if (length(value) != 2L) stop(GiottoUtils::wrap_txt(
      "When value is provided as numeric/integer, input must be length of 2,",
      "designating nodes 'from' and 'to'"
    ))
    value = as.integer(value)

    if (add) {
      # rbind to append
      x@relations = rbind(
        x@relations,
        data.table::data.table(
          from = value[1],
          to = value[2]
        )
      )
      x@relations = unique(x@relations)
    } else {
      # replace
      x@relations = data.table::data.table(
        from = value[1],
        to = value[2]
      )
    }

    return(x)
  }
)

setMethod(
  'sankeyRelate<-',
  signature(x = 'giottoSankeyPlan', add = 'logical', value = 'character'),
  function(x, add, value)
  {
    if(length(x@node_label) == 0L) stop('No node labels found.')
    # match to node names
    node_ids = match(c(value[1], value[2]), x@node_label)
    sankeyRelate(x, add) = node_ids # pass to numeric
    return(x)
  }
)

setMethod( # provide default add behavior if missing
  'sankeyRelate<-',
  signature(x = 'giottoSankeyPlan', add = 'missing', value = 'ANY'),
  function(x, value)
  {
    sankeyRelate(x, add = TRUE) = value
    return(x)
  }
)

setMethod( # remove all
  'sankeyRelate<-',
  signature(x = 'giottoSankeyPlan', add = 'missing', value = 'NULL'),
  function(x, value)
  {
    x@relations = NULL
    x
  }
)

# labels ####
sankeyLabel = function(x) {
  return(x@node_label)
}

`sankeyLabel<-` = function(x, value) {
  x@node_label = value
  return(x)
}


# Multiple references to the same address are not allowed
# e1 values will be taken in all such cases
setMethod('+', signature('giottoSankeyPlan'), function(e1, e2) {

  # update addresses
  e1@node_address = rbind(e1@node_address, e2@node_address)
  if (any(duplicated(e1@node_address[, c('spat_unit', 'feat_type', 'col')]))) {
    stop('Not possible to append more than one reference to the same node')
    # TODO try to recover
  }

  # update relations
  if(nrow(e2@relations) > 0L) {
    e1_n_nodes = nrow(e1@node_address)
    e2_rels = data.table::copy(e2@relations)
    e2_rels[, from := from + e1_n_nodes]
    e2_rels[, to := to + e1_n_nodes]

    e1@relations = rbind(e1@relations, e2_rels)
  }


  # update labels
  e1@node_label = c(e1@node_label, e2@node_label)

  # update subsets
  e1@node_subset = c(e1@node_subset, e2@node_subset)

  e1
})




# functions ####



#' @name sankeySet
#' @title Create a `giottoSankeyPlan` with one set of annotations
#' @param spat_unit spatial unit of the metadata
#' @param feat_type feature type of the metadata
#' @param col_name which column of metadata to pull from. Must be data that
#' can be treated categorically.
#' @param index character, integer, or logical vector to subset metadata table
#' @export
#' @keywords plotting sankey
sankeySet = function(spat_unit = NULL, feat_type = NULL, col, index = NULL, label = NA_character_) {
  x = giottoSankeyPlan(
    node_address = data.table::data.table(
      spat_unit = spat_unit,
      feat_type = feat_type,
      col = col
    ),
    node_subset = list(index)
  )
  x@node_label = label
  x
}


#' @title Apply subset index on a sankey set
#' @name subsetSankeySet
#' @param x giottoSankeyPlan object
#' @param set_id set index or node label
#' @param index new index subset, provided in the same order as the set_id
#' @export
#' @keywords plotting sankey
subsetSankeySet = function(x, set_id, index = list()) {
  if (!is.list(index)) index = list(index)
  if (is.character(set_id)) set_id = match(set_id, x@node_label)

  if (!set_id %in% seq(nrow(x@node_address)))
    stop('provided set id is not in the giottoSankeyPlan object')

  for(i in seq_along(set_id)) {
    x@node_subset[i] = index[i]
  }
  x
}


#' @title Data.table of sankey set locations
#' @name sankeySetAddresses
#' @description Return a data.table containing where the sets of metadata to
#' relate to each other exists inside the Giotto object.
#' @param x giottoSankeyPlan object
#' @export
#' @keywords plotting sankey
sankeySetAddresses = function(x) {
  combined_dt <- cbind(x@node_address, x@node_subset)
  colnames(combined_dt)[4] = 'index'
  combined_dt
}




#' @title Calculations for a sankey relationship pair
#' @name sankey_compare
#' @description
#' Generate the data.table of source, target, and value for a relation pair as
#' well as the list of node names to be used with sankey plotting.
#' @param data_dt data.table with two columns. Each should contain
#' categorical data.
#' @param idx_start starting index of nodes
#' @keywords internal
#' @return list with 1. node names and 2. data.table with cols source, target,
#' and value
sankey_compare = function(data_dt, idx_start = 0) {

  # Produce data.table of source, target, and value for the relation pair
  # This is a table of [links] between nodes
  c_names = colnames(data_dt)
  links = data.table::as.data.table(
    data_dt[, table(get(c_names[1]), get(c_names[2]))]
  )
  data.table::setnames(links, new = c('source', 'target', 'value'))

  # Remove any rows where value is 0. No need to include links between groups
  # that are not substantiated
  links = links[value > 0L]

  # Collect unique node names
  # These node names now define [nodes] to map the data.table source and target
  # column values to.
  source_names = links[, unique(source)]
  target_names = links[, unique(target)]

  # Set starting indices for each of the nodes
  source_idx_start = idx_start
  target_idx_start = length(source_names)

  # Convert source and target columns to integer mappings to unique names.
  # !These integer mappings are zero indexed!
  links[, source := match(source, source_names) - 1 + source_idx_start]
  links[, target := match(target, target_names) - 1 + target_idx_start]

  # combine node values into single character vector, starting with source
  # nodes. Additionally, ensure nodes are of type character.
  nodes = c(source_names, target_names) %>%
    as.character()

  # return data.table of links and the character vector of nodes
  return_list = list(
    links = links,
    nodes = nodes
  )

  return(return_list)
}





#' @name sankey_relation_pair
#' @title Calculations for a sankey relationship pair
#' @description
#' Get matched values to compare from the giotto object. Comparison columns
#' are then passed as a data.table to `sankey_compare` to be calculated.
#' @param g giotto object
#' @param gsp giottoSankeyPlan object
#' @param rel_idx index of relation pair in `gsp`
sankey_relation_pair = function(g, gsp, rel_idx, node_idx_start = 0) {

  rel = gsp@relations[rel_idx]
  from_address = sankeySetAddresses(gsp)[rel$from + 1]
  to_address = sankeySetAddresses(gsp)[rel$to + 1]

  # Data type being compared. Either cell or feat
  data_type = gsp@data_type
  if (is.null(data_type)) data_type = 'cell'
  # Determines which type of metadata to get
  meta_get_fun = switch(
    data_type,
    'cell' = getCellMetadata,
    'feat' = getFeatureMetadata
  )
  id_col = switch(
    data_type,
    'cell' = 'cell_ID',
    'feat' = 'feat_ID'
  )

  # get metadata
  # Defaults for spat_unit and feat_type are set inside of the getter if they
  # are provided as NULL
  meta_from = meta_get_fun(
    gobject = g,
    spat_unit = from_address$spat_unit,
    feat_type = from_address$feat_type,
    output = 'cellMetaObj',
    copy_obj = TRUE,
    set_defaults = TRUE
  )
  meta_to = meta_get_fun(
    gobject = g,
    spat_unit = to_address$spat_unit,
    feat_type = to_address$feat_type,
    output = 'cellMetaObj',
    copy_obj = TRUE,
    set_defaults = TRUE
  )

  # perform subset
  # 1. subobject subsetting does not drop ID col
  # 2. drop to data.table
  # 3. DT key on id col so that join is easier to perform
  idx_from = unlist(from_address$index)
  idx_to = unlist(to_address$index)
  if (!is.null(idx_from)) {
    meta_from = meta_from[idx_from]
  }
  if (!is.null(idx_to)) {
    meta_to = meta_to[idx_to]
  }
  meta_from = meta_from[,from_address$col][]
  meta_to = meta_to[,to_address$col][]
  data.table::setkeyv(meta_from, id_col)
  data.table::setkeyv(meta_to, id_col)

  # DT join on ids, then remove ids
  test_dt = meta_from[meta_to]
  test_dt[, (id_col) := NULL]

  res = sankey_compare(data_dt = test_dt,
                       idx_start = node_idx_start)

  return(res)
}








#' @title Create a sankey plot
#' @name sankeyPlot
#' @description
#' Create a sankey plot from a giotto object. Pulls from information in the
#' metadata.
#' @inheritParams data_access_params
#' @inheritDotParams networkD3::sankeyNetwork -Links -Nodes -Source -Target -Value -NodeID
#' @param sankey_plan giottoSankeyPlan object
#' @param meta_type build sankey on cell or feature metadata
#' @examples
#' g = GiottoData::loadGiottoMini("vizgen")
#' leiden = sankeySet(spat_unit = 'aggregate',
#'                    feat_type = 'rna',
#'                    col = 'leiden_clus')
#' louvain = sankeySet(spat_unit = 'aggregate',
#'                     feat_type = 'rna',
#'                     col = 'louvain_clus')
#' plan = leiden + louvain
#' sankeyPlot(g, plan)
#' @export
#' @keywords plotting sankey
sankeyPlot = function(gobject,
                      sankey_plan,
                      meta_type = c('cell', 'feat'),
                      ...) {
  checkmate::assert_class(gobject, 'giotto')
  GiottoUtils::package_check("networkD3")
  meta_type = match.arg(meta_type, choices = c('cell', 'feat'))
  sankey_plan@data_type = meta_type

  # iterate through sankey relations in the giottoSankeyPlan
  node_idx_start = 0
  links_dt = data.table::data.table()
  nodes = c()

  for (rel_i in seq(nrow(sankeyRelate(sankey_plan)))) {

    rel_data = sankey_relation_pair(
      g = gobject,
      gsp = sankey_plan,
      rel_idx = rel_i,
      node_idx_start = node_idx_start
    )

    # append data
    links_dt = rbind(links_dt, rel_data$links)
    nodes = c(nodes, rel_data$nodes)

    # update start index
    node_idx_start = links_dt[, max(target)]
  }

  # create nodes table
  nodes = data.table::data.table(name = nodes)

  networkD3::sankeyNetwork(
    Links = links_dt,
    Nodes = nodes,
    Source = 'source',
    Target = 'target',
    Value = 'value',
    NodeID = 'name',
    ...
  )
}


# internals ####

