package net.p3pp3rf1y.sophisticatedbackpacks.command;

import net.minecraft.commands.CommandBuildContext;
import net.minecraft.commands.arguments.item.ItemArgument;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceKey;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.Arrays;
import java.util.Collection;

public class BackpackItemArgumentType extends ItemArgument {
	private static final Collection<String> EXAMPLES = Arrays.asList("backpack", "sophisticatedbackpacks:backpack", "sophisticatedbackpacks:backpack{foo=bar}");

	static CommandBuildContext backpackContext(final CommandBuildContext contextDelegate) {
		return new CommandBuildContext() {
			@Override
			public <T> HolderLookup<T> holderLookup(ResourceKey<? extends Registry<T>> resourceKey) {
				return contextDelegate.holderLookup(resourceKey).filterElements(item -> item instanceof BackpackItem);
			}
		};
	}

	public BackpackItemArgumentType(CommandBuildContext context) {
		super(backpackContext(context));
	}

	public static BackpackItemArgumentType item(CommandBuildContext context) {
		return new BackpackItemArgumentType(context);
	}

	public Collection<String> getExamples() {
		return EXAMPLES;
	}
}
