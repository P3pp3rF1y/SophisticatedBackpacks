package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.CommandDispatcher;
import com.mojang.brigadier.tree.LiteralCommandNode;
import net.minecraft.command.CommandSource;
import net.minecraft.command.Commands;
import net.minecraft.command.arguments.ArgumentSerializer;
import net.minecraft.command.arguments.ArgumentTypes;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

public class SBPCommand {
	private static final int OP_LEVEL = 2;

	private SBPCommand() {}

	public static void register(CommandDispatcher<CommandSource> dispatcher) {
		LiteralCommandNode<CommandSource> mainNode = dispatcher.register(
				Commands.literal("sbp")
						.requires(cs -> cs.hasPermission(OP_LEVEL))
						.then(ListCommand.register())
						.then(GiveCommand.register())
						.then(RemoveNonPlayerCommand.register())
		);
		dispatcher.register(Commands.literal("sophisticatedbackpacks").requires(cs -> cs.hasPermission(OP_LEVEL)).redirect(mainNode));
	}

	public static void registerArgumentTypes() {
		ArgumentTypes.register(RegistryHelper.getModRegistryName("backpack_uuid"), BackpackUUIDArgumentType.class, new ArgumentSerializer<>(BackpackUUIDArgumentType::backpackUuid));
		ArgumentTypes.register(RegistryHelper.getModRegistryName("player_name"), BackpackPlayerArgumentType.class, new ArgumentSerializer<>(BackpackPlayerArgumentType::playerName));
	}
}
