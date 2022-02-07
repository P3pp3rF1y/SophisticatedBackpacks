package net.p3pp3rf1y.sophisticatedcore.api;

import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;

import java.util.Optional;

public interface IIOFilterUpgrade {
	Optional<FilterLogic> getInputFilter();

	Optional<FilterLogic> getOutputFilter();
}
