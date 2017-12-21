function unixTimeStamp returns decimal():
	return time + (integer(today - 2440589) * 86400).
end function.
