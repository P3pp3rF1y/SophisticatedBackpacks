package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;

public class RemoveNonPlayerCommand {
	private RemoveNonPlayerCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSourceStack, ?> register() {
		return Commands.literal("removeNonPlayer")
				.then(Commands.argument("onlyWithEmptyInventory", BoolArgumentType.bool())
						.executes(context -> removeNonPlayerBackpacks(context.getSource(), BoolArgumentType.getBool(context, "onlyWithEmptyInventory"))));
	}

	private static int removeNonPlayerBackpacks(CommandSourceStack source, boolean onlyWithEmptyInventory) {
		int numberRemoved = BackpackStorage.get().removeNonPlayerBackpackContents(onlyWithEmptyInventory);
		source.sendSuccess(Component.translatable("commands.sophisticatedbackpacks.remove_non_player.success", numberRemoved), false);
		return 0;
	}
}
