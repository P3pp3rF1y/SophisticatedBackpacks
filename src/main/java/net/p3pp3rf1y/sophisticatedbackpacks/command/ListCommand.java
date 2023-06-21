package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.builder.ArgumentBuilder;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.ClickEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.HoverEvent;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.network.chat.TextColor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.AccessLogRecord;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

public class ListCommand {
	private ListCommand() {}

	@SuppressWarnings("java:S1452")
	static ArgumentBuilder<CommandSourceStack, ?> register() {
		return Commands.literal("list")
				.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getAllBackpackLogs()), context.getSource()))
				.then(Commands.argument("playerName", BackpackPlayerArgumentType.playerName())
						.executes(context -> printBackpackList(new ArrayList<>(BackpackAccessLogger.getBackpackLogsForPlayer(context.getArgument("playerName", String.class))), context.getSource()))
				);
	}

	private static int printBackpackList(List<AccessLogRecord> allLogs, CommandSourceStack source) {
		SimpleDateFormat dateFormat = new SimpleDateFormat();
		allLogs.sort(Comparator.comparing(AccessLogRecord::getAccessTime).reversed());
		source.sendSuccess(() -> Component.translatable("commands.sophisticatedbackpacks.list.header"), false);
		allLogs.forEach(alr -> {
			MutableComponent message = Component.literal("");
			message.append(Component.literal(alr.getBackpackName())
					.withStyle(s ->
							s.withColor(ChatFormatting.GREEN).withClickEvent(new ClickEvent(ClickEvent.Action.SUGGEST_COMMAND, "/sophisticatedbackpacks give @p " + alr.getBackpackUuid()))
									.withHoverEvent(new HoverEvent(HoverEvent.Action.SHOW_TEXT, Component.translatable("chat.sophisticatedbackpacks.backpack_uuid.tooltip", alr.getBackpackUuid())))
					)
			);
			message.append(Component.literal(", "));
			MutableComponent clothColor = Component.translatable("commands.sophisticatedbackpacks.list.cloth_color");
			clothColor.withStyle(clothColor.getStyle().withColor(TextColor.fromRgb(alr.getClothColor())));
			message.append(clothColor);
			message.append(Component.literal(" "));
			MutableComponent trimColor = Component.translatable("commands.sophisticatedbackpacks.list.trim_color");
			trimColor.withStyle(trimColor.getStyle().withColor(TextColor.fromRgb(alr.getTrimColor())));
			message.append(trimColor);
			message.append(Component.literal(", "));
			message.append(Component.literal(alr.getPlayerName()));
			message.append(Component.literal(", "));
			message.append(Component.literal(dateFormat.format(new Date(alr.getAccessTime()))));
			source.sendSuccess(() -> message, false);
		});
		return 0;
	}
}
