regiongrid <- data.frame(
  name = c("Region 9", "Region 11", "Region 1", "Region 10", "Region 8", "Region 2", "Region 7", "Region 3", "Region 6", "Region 4", "Region 5"),
  code = c("9", "11", "1", "10", "8", "2", "7", "3", "6", "4", "5"),
  row = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5),
  col = c(2, 3, 1, 3, 2, 2, 3, 2, 3, 2, 3),
  stringsAsFactors = FALSE
)
#geofacet::grid_preview(regiongrid)
write_csv(regiongrid,"regiongrid.csv")



region 1: Jo Davies, Stephenson, Winnebago, Boone, Dekalb, Carrol, Ogle, Whiteside, Lee, Crawford

region 2: Rock Island, Henry, Bureau, Putnam, Kendall, Grundy, Mercer, Knox, Henderson, Warren, McDonough, Fulton, Stark, Marshall, Peoria, Tazwell, McLean, Woodford, Livingston, Lasalle

region 3: Hancock, Adams, Pike, Calhoun, Jersey, Greene, Scott, Brown, Schuyler, Cass, Morgan, Macoupin, Montgomery, Christian, Sangamon, Logan, Menard, Mason

region 4: Bond, Madison, St. Clair, Clinton, Washington, Monroe, Randolph

region 5: Marion, Jefferson, Wayne, Edwards, Wabash, Perry, Jackson, Franklin, Williamson, Saline, Hamilton, White, Gallatin, Union, Johnson, Pope, Hardin, Alexander, Massac, Pulaski

region 6: Iroquois, Ford, Dewitt, Piatt, Champaign, Vermillion, Macon, Moultrie, Douglas, Edgar, Shelby, Coles, Cumberland, Clark, Fayette, Effingham, Jasper, Crawford, Clay, Richland, Lawrence

region 7: Will, Kankakee

region 8: Kane, Dupage

region 9: McHenry, Lake

region 10: Cook

region 11: Chicago