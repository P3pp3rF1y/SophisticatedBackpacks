package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;

import java.util.Optional;

public interface IIOFilterUpgrade {
	Optional<FilterLogic> getInputFilter();

	Optional<FilterLogic> getOutputFilter();
}
