package net.p3pp3rf1y.sophisticatedbackpacks.command;

import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.arguments.item.ItemArgument;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;

import java.util.Arrays;
import java.util.Collection;

public class BackpackUpgradeItemArgumentType extends ItemArgument {
	private static final Collection<String> EXAMPLES = Arrays.asList("stack_upgrade_tier_1", "sophisticatedbackpacks:stack_upgrade_tier_1");

	static CommandBuildContext upgradeItemContext(final CommandBuildContext contextDelegate) {
		return new CommandBuildContext() {
			@Override
			public <T> HolderLookup<T> holderLookup(ResourceKey<? extends Registry<T>> resourceKey) {
				return contextDelegate.holderLookup(resourceKey).filterElements(item -> item instanceof UpgradeItemBase<?>);
			}
		};
	}

	public BackpackUpgradeItemArgumentType(CommandBuildContext context) {
		super(upgradeItemContext(context));
	}

	public static BackpackUpgradeItemArgumentType item(CommandBuildContext context) {
		return new BackpackUpgradeItemArgumentType(context);
	}

	public Collection<String> getExamples() {
		return EXAMPLES;
	}
}
